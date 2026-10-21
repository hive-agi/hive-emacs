(ns hive-emacs.daemon-liveness
  "Liveness-first heartbeat: probe the daemon locally, spawn emacsclient only
   when the probe cannot vouch for it.

   The heartbeat loop used to run one emacsclient per active daemon every
   30 s. A healthy daemon whose Emacs pid is alive gains nothing from that
   round-trip, so it now costs one ProcessHandle lookup. emacsclient is used
   when health drops: the pid is unknown or gone, the daemon has errors or is
   not :healthy, or the last real ping is older than `confirm-interval-ms`
   (a frozen Emacs keeps its pid alive, so a pid alone is never trusted
   forever). The ping evaluates `(emacs-pid)`, so the first ping also teaches
   the probe which pid to watch.

   Ports (all injected, see `heartbeat!`): ping-fn (daemon-id -> PingResult),
   liveness-fn (daemon entity -> :alive | :dead | :unknown), now-fn
   (() -> java.util.Date)."
  (:require [clojure.string :as str]
            [hive-emacs.daemon :as daemon]
            [hive-emacs.daemon-heartbeat :as heartbeat]
            [hive-emacs.daemon-scoring :as scoring]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def confirm-interval-ms
  "Longest a live pid alone vouches for a healthy daemon before a real ping."
  (* 10 60 1000))

(def ping-elisp
  "What the ping evaluates: as cheap as `t`, and it answers the pid."
  "(emacs-pid)")

;; -- pure ---------------------------------------------------------------------

(defn ping-needed?
  "True when the cheap PROBE cannot vouch for ENTITY at NOW-MS."
  [entity probe now-ms]
  (let [score (or (:emacs-daemon/health-score entity) scoring/default-health-score)
        errors (or (:emacs-daemon/error-count entity) 0)
        pinged-ms (some-> ^java.util.Date (:emacs-daemon/pinged-at entity) .getTime)]
    (boolean
     (or (not= :alive probe)
         (pos? errors)
         (not= :healthy (scoring/health-level score))
         (nil? pinged-ms)
         (>= (- now-ms pinged-ms) confirm-interval-ms)))))

(defn learned-pid
  "The Emacs pid a ping answered, or nil."
  [ping-result]
  (let [r (:result ping-result)]
    (cond
      (integer? r) (long r)
      (string? r) (some-> (re-find #"^\d+$" (str/trim r)) parse-long)
      :else nil)))

;; -- probe --------------------------------------------------------------------

(defn pid-liveness
  "Default liveness-fn: :alive when the daemon's recorded pid is a live
   process, :dead when it is not, :unknown when no pid was learned yet."
  [{:emacs-daemon/keys [pid]}]
  (if-not (integer? pid)
    :unknown
    (let [handle (java.lang.ProcessHandle/of (long pid))]
      (if (and (.isPresent handle)
               (.isAlive ^java.lang.ProcessHandle (.get handle)))
        :alive
        :dead))))

;; -- shell --------------------------------------------------------------------

(defn- ping!
  [store daemon-id ping-fn now]
  (let [answer (atom nil)
        report (heartbeat/heartbeat! store daemon-id
                                     (fn [id] (reset! answer (ping-fn id))))]
    (when (and report (:success? report))
      (let [pid (learned-pid @answer)]
        (daemon/update-daemon! store daemon-id
                               (cond-> {:emacs-daemon/pinged-at now}
                                 pid (assoc :emacs-daemon/pid pid)))))
    (some-> report (assoc :probe :ping))))

(defn- vouch!
  [store daemon-id entity now]
  (let [score (or (:emacs-daemon/health-score entity) scoring/default-health-score)
        lings (count (or (:emacs-daemon/lings entity) #{}))]
    (daemon/update-daemon! store daemon-id {:emacs-daemon/heartbeat-at now
                                            :emacs-daemon/status :active})
    (assoc (scoring/heartbeat-report {:success? true :latency-ms nil :new-errors 0}
                                     score lings)
           :probe :liveness)))

(defn heartbeat!
  "One liveness-first heartbeat for DAEMON-ID. Returns the heartbeat report
   with :probe (:liveness when no emacsclient ran, :ping when one did), or
   nil for an unknown daemon."
  [store daemon-id {:keys [ping-fn liveness-fn now-fn]}]
  (when-let [entity (daemon/get-daemon store daemon-id)]
    (let [now (now-fn)
          probe (try (liveness-fn entity) (catch Exception _ :unknown))]
      (if (ping-needed? entity probe (.getTime ^java.util.Date now))
        (ping! store daemon-id ping-fn now)
        (vouch! store daemon-id entity now)))))

(defn heartbeat-active!
  "Heartbeat every :active daemon (or DEFAULT-ID when none is registered
   but it exists). Returns the reports, in order."
  [store default-id opts]
  (let [all (daemon/get-all-daemons store)
        ids (if (seq all)
              (->> all
                   (filter #(= :active (:emacs-daemon/status %)))
                   (map :emacs-daemon/id))
              (when (daemon/get-daemon store default-id) [default-id]))]
    (into [] (keep #(heartbeat! store % opts)) ids)))
