(ns hive-emacs.liveness
  "Emacs liveness: a probe port, a pure circuit breaker over its results, and
   the gate every emacsclient call passes first.

   Why: on 2026-11-20 the Emacs main thread spent 95% of wall time in GC and
   read nothing from its sockets. Every cider spawn waited 94 s and failed with
   a generic message, and nothing told the swarm why (card
   20261120002307-19cf4206). Now one probe answers within 3 s, its result is
   reused for 10 s, and while it fails every emacsclient-backed verb refuses
   at once with the pid, the time since the last good probe, the cause
   (GC thrash, busy or down) and the fallback.

   Shape (CPPB):
   - probe port: (probe-fn) -> ProbeResult, injected by `install!`. The
     production probe lives in hive-emacs.client (`probe-emacs!`), the only
     namespace that runs emacsclient.
   - breaker: `step` is a pure function of (state, probe result, now).
   - gate: `admit` is the shell: it probes when the cached result is stale,
     steps the breaker, reports transitions, and answers a refusal or nil.

   ProbeResult: {:ok? true :pid n :gc-elapsed seconds :gcs-done n}
             or {:ok? false :error string :timed-out? bool}."
  (:require [clojure.string :as str]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def probe-timeout-ms
  "Longest the production probe waits for Emacs to answer `t`."
  3000)

(def probe-ttl-ms
  "How long one probe result is reused before the next call probes again.
   Bounds how fast a closed breaker notices a hang and how fast an open one
   notices recovery (at most ttl + probe timeout)."
  10000)

(def gc-thrash-fraction
  "Share of wall time spent in GC, between two good probes, from which the
   verdict says GC thrash. The 2026-11-20 incident measured 0.95."
  0.5)

;;; ---------------------------------------------------------------------------
;;; Pure
;;; ---------------------------------------------------------------------------

(defn initial-state
  []
  {:state :closed
   :failures 0
   :probed-at nil
   :last-probe nil
   :last-ok nil
   :opened-at nil
   :gc-fraction nil
   :cause nil})

(defn parse-probe-output
  "The probe's printed answer `(PID GC-ELAPSED GCS-DONE)` as a map, or nil."
  [s]
  (when-let [[_ pid gc n] (re-find #"\(\s*(\d+)\s+([0-9.eE+-]+)\s+(\d+)\s*\)" (str s))]
    {:pid (parse-long pid)
     :gc-elapsed (parse-double gc)
     :gcs-done (parse-long n)}))

(defn gc-fraction
  "Share of wall time Emacs spent in GC between the good probes PREV (taken at
   PREV-AT ms) and CUR (at NOW ms); nil when it cannot be measured (no
   previous probe, another Emacs pid, counters that went backwards)."
  [prev prev-at cur now]
  (let [wall-s (/ (- now (or prev-at now)) 1000.0)
        dgc (when (and (:gc-elapsed prev) (:gc-elapsed cur))
              (- (:gc-elapsed cur) (:gc-elapsed prev)))]
    (when (and dgc (pos? wall-s) (>= dgc 0)
               (= (:pid prev) (:pid cur)))
      (min 1.0 (/ dgc wall-s)))))

(defn cause
  "Why the breaker is open: :gc-thrash when the last good probes showed GC
   eating the wall clock, :busy when the probe timed out, :down otherwise
   (socket gone, connection refused)."
  [st probe]
  (cond
    (>= (or (:gc-fraction st) 0.0) gc-thrash-fraction) :gc-thrash
    (:timed-out? probe) :busy
    :else :down))

(defn step
  "Pure breaker transition on PROBE taken at NOW ms. Returns
   {:state new-state :transition :opened|:closed|nil}. One failed probe opens
   the breaker (it already waited the probe timeout); one good probe closes
   it."
  [st probe now]
  (let [before (:state st)
        st (assoc st :probed-at now :last-probe (assoc probe :at now))
        st (if (:ok? probe)
             (let [frac (gc-fraction (:last-ok st) (get-in st [:last-ok :at]) probe now)]
               (assoc st
                      :state :closed
                      :failures 0
                      :opened-at nil
                      :cause nil
                      :last-ok (assoc probe :at now)
                      :gc-fraction (or frac (when (= (:pid probe) (get-in st [:last-ok :pid]))
                                              (:gc-fraction st)))))
             (assoc st
                    :state :open
                    :failures (inc (:failures st 0))
                    :opened-at (or (:opened-at st) now)
                    :cause (cause st probe)))
        after (:state st)]
    {:state st
     :transition (cond
                   (and (= :closed before) (= :open after)) :opened
                   (and (= :open before) (= :closed after)) :closed)}))

(defn stale?
  "True when ST holds no probe result younger than `probe-ttl-ms` at NOW."
  [st now]
  (or (nil? (:probed-at st))
      (>= (- now (:probed-at st)) probe-ttl-ms)))

(defn verdict
  "The breaker as data for status surfaces."
  [st now]
  (let [last-ok-at (get-in st [:last-ok :at])]
    (cond-> {:state (:state st)
             :pid (get-in st [:last-ok :pid])
             :failures (:failures st)
             :probe-ttl-ms probe-ttl-ms
             :probe-timeout-ms probe-timeout-ms}
      (:cause st) (assoc :cause (:cause st))
      (:gc-fraction st) (assoc :gc-fraction (:gc-fraction st))
      (:gc-fraction st) (assoc :gc-thrash? (>= (:gc-fraction st) gc-thrash-fraction))
      last-ok-at (assoc :seconds-since-good-probe (quot (- now last-ok-at) 1000))
      (:probed-at st) (assoc :probe-age-ms (- now (:probed-at st)))
      (:error (:last-probe st)) (assoc :last-error (:error (:last-probe st))))))

(defn- cause-text
  [st]
  (case (:cause st)
    :gc-thrash (format "GC thrash: %.0f%% of wall time in GC between the last good probes"
                       (* 100.0 (or (:gc-fraction st) 0.0)))
    :busy "busy: the main loop did not answer `t`"
    :down "down: the server socket did not accept the probe"
    "unknown"))

(defn refusal-message
  "The refusal every gated call answers while the breaker is open."
  [st now]
  (let [{:keys [pid seconds-since-good-probe last-error]} (verdict st now)]
    (str "Emacs is not answering (pid " (or pid "unknown") ", " (cause-text st) "; "
         (if seconds-since-good-probe
           (str seconds-since-good-probe " s since the last good probe")
           "no good probe since hive-emacs loaded")
         (when-not (str/blank? last-error) (str "; probe: " (str/trim last-error)))
         "). emacsclient-backed verbs (cider spawn, eval, sessions, emacs ...) are refused "
         "until a liveness probe answers; Emacs is re-probed every "
         (quot probe-ttl-ms 1000) " s. Fallback: evaluate over a direct nREPL port "
         "(`code cider eval port=<n>` or clojure_eval with port), or run the tests "
         "cold (hive-clj -J-Xmx2g -M:test -n <ns>).")))

;;; ---------------------------------------------------------------------------
;;; Shell
;;; ---------------------------------------------------------------------------

(defonce ^:private config (atom nil))

(defonce ^:private breaker (atom (initial-state)))

(defonce ^:private probe-lock (java.util.concurrent.locks.ReentrantLock.))

(defn- log-transition
  [transition st]
  (if (= :opened transition)
    (log/warn "Emacs liveness breaker OPEN" (verdict st (System/currentTimeMillis)))
    (log/info "Emacs liveness breaker closed" (verdict st (System/currentTimeMillis)))))

(defn install!
  "Arm the gate. PROBE-FN: (fn [] ProbeResult), the only way the gate reaches
   Emacs. NOW-FN: (fn [] epoch-ms), default the wall clock. ON-TRANSITION:
   (fn [transition state]) called once per :opened / :closed transition.
   Resets the breaker."
  [{:keys [probe-fn now-fn on-transition]}]
  {:pre [(fn? probe-fn)]}
  (reset! breaker (initial-state))
  (reset! config {:probe-fn probe-fn
                  :now-fn (or now-fn #(System/currentTimeMillis))
                  :on-transition (or on-transition log-transition)})
  nil)

(defn uninstall!
  "Disarm the gate: every call is admitted again."
  []
  (reset! config nil)
  (reset! breaker (initial-state))
  nil)

(defn installed?
  []
  (some? @config))

(defn- run-probe
  [probe-fn]
  (try
    (let [r (probe-fn)]
      (if (map? r) r {:ok? false :error (str "probe answered " (pr-str r))}))
    (catch Throwable t
      {:ok? false :error (str "probe threw: " (ex-message t))})))

(defn check!
  "The breaker state, probing first when the cached result is stale. Only one
   caller probes at a time; the others answer from the current state, so no
   caller waits behind another's probe. Nil when the gate is not installed."
  []
  (when-let [{:keys [probe-fn now-fn on-transition]} @config]
    (when (and (stale? @breaker (now-fn))
               (.tryLock ^java.util.concurrent.locks.ReentrantLock probe-lock))
      (try
        (when (stale? @breaker (now-fn))
          (let [probe (run-probe probe-fn)
                {:keys [state transition]} (step @breaker probe (now-fn))]
            (reset! breaker state)
            (when transition
              (try (on-transition transition state)
                   (catch Throwable t
                     (log/debug "liveness transition hook failed" (ex-message t)))))))
        (finally (.unlock ^java.util.concurrent.locks.ReentrantLock probe-lock))))
    @breaker))

(defn admit
  "Nil when an emacsclient call may proceed; else the refusal response
   {:success false :error message :emacs-unresponsive true :liveness verdict}.
   Admits everything when the gate is not installed."
  []
  (when-let [st (check!)]
    (when (= :open (:state st))
      (let [now ((:now-fn @config))]
        {:success false
         :error (refusal-message st now)
         :emacs-unresponsive true
         :liveness (verdict st now)}))))

(defn invalidate!
  "Forget the cached probe result, so the next gated call probes first. The
   client calls it when an admitted emacsclient call times out: a hang that
   began inside the cache window is then caught by the next call, not ten
   seconds later."
  []
  (swap! breaker assoc :probed-at nil)
  nil)

(defn status
  "The breaker as data, without probing. Nil when the gate is not installed."
  []
  (when-let [{:keys [now-fn]} @config]
    (verdict @breaker (now-fn))))
