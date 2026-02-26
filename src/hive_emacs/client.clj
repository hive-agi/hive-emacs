(ns hive-emacs.client
  "Shell wrapper for emacsclient communication with running Emacs.

   3-state circuit breaker (closed/open/half-open) with exponential backoff
   and structured telemetry prevent cascading failures when Emacs dies.
   structured data for observability."
  (:require [clojure.java.shell :refer [sh]]
            [clojure.string :as str]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:dynamic *emacsclient-path*
  "Path to emacsclient binary."
  (or (System/getenv "EMACSCLIENT") "emacsclient"))

(def ^:dynamic *emacs-socket-name*
  "Emacs daemon socket name. When set, emacsclient calls include `-s <name>`.
   Reads from EMACS_SOCKET_NAME env var. nil = default daemon (no -s flag)."
  (System/getenv "EMACS_SOCKET_NAME"))

(def ^:dynamic *default-timeout-ms*
  "Default timeout for emacsclient calls in milliseconds."
  5000)

(def ^:dynamic *max-timeout-ms*
  "Hard ceiling for any emacsclient call. The client layer is the last line of
   defense — no call may exceed this regardless of what callers request."
  30000)

(def ^:private daemon-dead-patterns
  "Stderr patterns from emacsclient that indicate daemon death.
   Each is [regex-pattern keyword-tag] for structured telemetry."
  [[#"(?i)can't find socket"           :socket-not-found]
   [#"(?i)No such file or directory"   :socket-missing]
   [#"(?i)Connection refused"          :connection-refused]
   [#"(?i)connection reset"            :connection-reset]
   [#"(?i)server did not respond"      :server-unresponsive]
   [#"(?i)socket.*not available"       :socket-unavailable]])

;; ---------------------------------------------------------------------------
;; 3-State Circuit Breaker
;;
;; States:
;;   :closed    — Normal operation. All calls proceed.
;;   :open      — Emacs confirmed dead. Calls are BLOCKED with sentinel.
;;                Exponential backoff: 1s → 2s → 4s → ... → max 60s.
;;                After backoff period elapses, transitions to :half-open.
;;   :half-open — Probe state. ONE call is allowed through.
;;                If it succeeds → :closed (recovery).
;;                If it fails   → :open (reset backoff).
;;
;; This is the PRIMARY FIX for the GC death spiral:
;; Previously the breaker was SET but never CHECKED, so when Emacs was down,
;; every heartbeat spawned a future → sh process → failure → repeat,
;; creating 108K core.async channels → OOM.
;; ---------------------------------------------------------------------------

(def ^:const initial-backoff-ms
  "Initial backoff duration when circuit opens. Doubles on each failure."
  1000)

(def ^:const max-backoff-ms
  "Maximum backoff duration. Caps exponential growth."
  60000)

(defonce ^{:doc "3-state circuit breaker for emacsclient calls.
   :state          — :closed, :open, or :half-open
   :tripped-at     — System/currentTimeMillis when circuit last opened
   :backoff-ms     — current backoff duration (doubles on repeated failures)
   :crash-count    — total crash detections since JVM start
   :last-error     — last daemon-death stderr string
   :last-tag       — last daemon-death keyword tag
   :recovery-at    — last time circuit transitioned to :closed"}
  circuit-breaker
  (atom {:state :closed
         :tripped-at nil
         :backoff-ms initial-backoff-ms
         :crash-count 0
         :last-error nil
         :last-tag nil
         :recovery-at nil}))

(def ^:dynamic *circuit-breaker-cooldown-ms*
  "Minimum time (ms) the circuit stays open before a recovery probe is allowed.
   Prevents thundering-herd probes right after a crash."
  5000)

(defn circuit-breaker-state
  "Get the current circuit breaker state. Public for monitoring/testing."
  []
  @circuit-breaker)

(defn- transition-breaker!
  "Atomically transition the circuit breaker, logging the change.
   Returns the new state."
  [new-state-fn reason]
  (let [old @circuit-breaker
        new (swap! circuit-breaker new-state-fn)
        old-state (:state old)
        new-state (:state new)]
    (when (not= old-state new-state)
      (log/info "Circuit breaker:" (name old-state) "->" (name new-state)
                (when reason (str "(" reason ")"))))
    new))

(defn- trip-breaker!
  "Transition circuit breaker to :open state on failure.
   If already open, doubles the backoff (exponential backoff).
   If transitioning from :half-open, resets backoff to initial."
  [error-str death-tag]
  (transition-breaker!
   (fn [cb]
     (let [now (System/currentTimeMillis)
           prev-state (:state cb)
           new-backoff (case prev-state
                         ;; First failure or from half-open probe failure: start fresh
                         :closed    initial-backoff-ms
                         :half-open initial-backoff-ms
                         ;; Already open: double the backoff (exponential)
                         :open      (min max-backoff-ms
                                         (* 2 (or (:backoff-ms cb) initial-backoff-ms))))]
       (assoc cb
              :state :open
              :tripped-at now
              :backoff-ms new-backoff
              :crash-count (inc (:crash-count cb))
              :last-error error-str
              :last-tag death-tag)))
   (when death-tag (name death-tag))))

(defn- recover-breaker!
  "Transition circuit breaker to :closed state on successful call.
   Resets backoff to initial."
  []
  (transition-breaker!
   (fn [cb]
     (assoc cb
            :state :closed
            :backoff-ms initial-backoff-ms
            :recovery-at (System/currentTimeMillis)))
   "success"))

(defn- maybe-half-open!
  "If the circuit is :open and the backoff period has elapsed,
   atomically transition to :half-open to allow a probe call.
   Returns true if transition happened (caller should proceed with probe).
   Returns false if still in backoff (caller should skip)."
  []
  (let [cb @circuit-breaker
        now (System/currentTimeMillis)
        elapsed (- now (or (:tripped-at cb) 0))
        backoff (or (:backoff-ms cb) initial-backoff-ms)]
    (if (>= elapsed backoff)
      ;; Backoff elapsed — try to transition to :half-open atomically
      ;; Use compare-and-swap to ensure only ONE caller wins the probe
      (let [new-cb (assoc cb :state :half-open)]
        (if (compare-and-set! circuit-breaker cb new-cb)
          (do
            (log/info "Circuit breaker:" "open" "->" "half-open"
                      "(backoff" backoff "ms elapsed, probing)")
            true)
          ;; Lost the race — another thread already transitioned
          false))
      ;; Still in backoff period
      false)))

(defn reset-circuit-breaker!
  "Reset the circuit breaker to :closed state. For testing/manual recovery."
  []
  (reset! circuit-breaker
          {:state :closed
           :tripped-at nil
           :backoff-ms initial-backoff-ms
           :crash-count 0
           :last-error nil
           :last-tag nil
           :recovery-at nil}))

(defn- check-circuit-breaker
  "Check the circuit breaker BEFORE making an emacsclient call.
   This is THE critical guard that prevents the GC death spiral.

   Returns:
     :proceed   — Circuit is closed or half-open (probe). Make the call.
     :blocked   — Circuit is open and backoff hasn't elapsed. Skip the call."
  []
  (let [cb @circuit-breaker]
    (case (:state cb)
      :closed    :proceed
      :half-open :proceed
      :open      (if (maybe-half-open!)
                   :proceed
                   :blocked))))

;; ---------------------------------------------------------------------------
;; Core implementation
;; ---------------------------------------------------------------------------

(defn- detect-daemon-death
  "Check if an error string indicates daemon death.
   Returns [matched? tag] where tag is the specific death pattern matched."
  [error-str]
  (when (string? error-str)
    (some (fn [[pattern tag]]
            (when (re-find pattern error-str)
              [true tag]))
          daemon-dead-patterns)))

(defn- report-daemon-error!
  "Report a daemon error to the daemon store.
   Uses requiring-resolve for lazy loading to avoid cyclic dependencies.

   This is the integration point between emacsclient circuit breaker
   and the IEmacsDaemon lifecycle tracking system."
  [error-message death-tag]
  (try
    ;; Use requiring-resolve for lazy loading
    (when-let [mark-error! (requiring-resolve 'hive-emacs.daemon-store/mark-error!)]
      (when-let [default-daemon-id (requiring-resolve 'hive-emacs.daemon-store/default-daemon-id)]
        (let [daemon-id (default-daemon-id)]
          (mark-error! daemon-id (str "[" (name death-tag) "] " error-message))
          (log/info :daemon-error-reported {:daemon-id daemon-id
                                            :death-tag death-tag
                                            :message error-message}))))
    (catch Exception e
      ;; Don't fail the operation if reporting fails
      (log/debug "Could not report daemon error to store:" (.getMessage e)))))

(defn- unwrap-emacs-string
  "Unwrap emacsclient print format quoting.
   Emacs wraps string results in quotes: \"foo\" -> \"\\\"foo\\\"\"
   This undoes that wrapping for JSON and other string results."
  [s]
  (if (and (string? s)
           (>= (count s) 2)
           (str/starts-with? s "\"")
           (str/ends-with? s "\""))
    ;; Parse the outer quotes away using Clojure reader
    (try
      (read-string s)
      (catch Exception _ s))
    s))

(defn eval-elisp-with-timeout
  "Execute elisp code with a timeout. Returns immediately if the operation
   takes longer than timeout-ms milliseconds.
   Clamps timeout-ms to *max-timeout-ms* (30s) as a hard ceiling.

   CIRCUIT BREAKER GUARD: Checks breaker state BEFORE spawning any process.
   When breaker is :open and backoff hasn't elapsed, returns a sentinel
   failure immediately — no future, no sh process, no resource consumption.

   IEmacsDaemon integration: On daemon death detection (matching daemon-dead-patterns),
   reports the error to the daemon store for lifecycle tracking.

   Returns a map with :success, :result or :error keys.
   On timeout, returns {:success false :error \"Timeout...\" :timed-out true}
   On circuit-open, returns {:success false :error \"Circuit breaker open...\" :circuit-open true}"
  ([code] (eval-elisp-with-timeout code *default-timeout-ms*))
  ([code timeout-ms]
   ;; === CIRCUIT BREAKER GUARD ===
   ;; This check is the PRIMARY fix for the GC death spiral.
   ;; Without it, every call spawns a future → sh → failure → repeat.
   (let [breaker-decision (check-circuit-breaker)]
     (if (= breaker-decision :blocked)
       ;; Circuit is open — return sentinel immediately, no process spawned
       (let [cb @circuit-breaker
             remaining-ms (max 0 (- (or (:backoff-ms cb) initial-backoff-ms)
                                    (- (System/currentTimeMillis)
                                       (or (:tripped-at cb) 0))))]
         (log/debug :circuit-breaker-blocked
                    {:backoff-ms (:backoff-ms cb)
                     :remaining-ms remaining-ms
                     :crash-count (:crash-count cb)
                     :code-preview (subs code 0 (min 50 (count code)))})
         {:success false
          :error (format "Circuit breaker open (backoff %dms, %dms remaining). Last error: %s"
                         (or (:backoff-ms cb) initial-backoff-ms)
                         remaining-ms
                         (or (:last-error cb) "unknown"))
          :circuit-open true
          :duration-ms 0})

       ;; Circuit is closed or half-open — proceed with the call
       (let [half-open? (= :half-open (:state @circuit-breaker))
             timeout-ms (min (or timeout-ms *default-timeout-ms*) *max-timeout-ms*)
             _          (log/debug "Executing elisp with timeout:" timeout-ms "ms -" code
                                   (when half-open? "(half-open probe)"))
             start      (System/currentTimeMillis)
             f          (future
                          (try
                            (let [{:keys [exit out err]} (apply sh (cond-> [*emacsclient-path*]
                                                                     *emacs-socket-name* (conj "-s" *emacs-socket-name*)
                                                                     true (conj "--eval" code)))]
                              (if (zero? exit)
                                {:success true :result (unwrap-emacs-string (str/trim out))}
                                {:success false :error (str/trim err)}))
                            (catch Exception e
                              {:success false :error (str "Failed to execute emacsclient: " (.getMessage e))})))]
         (try
           (let [result (deref f timeout-ms ::timeout)
                 duration (- (System/currentTimeMillis) start)]
             (if (= result ::timeout)
               (do
                 (future-cancel f)
                 (log/warn :emacsclient-timeout {:timeout-ms timeout-ms
                                                  :code-preview (subs code 0 (min 100 (count code)))})
                 ;; Timeout counts as failure for circuit breaker
                 (when half-open?
                   (trip-breaker! (format "Timeout after %dms" timeout-ms) :timeout))
                 {:success false
                  :error (format "Emacsclient call timed out after %dms" timeout-ms)
                  :timed-out true
                  :duration-ms duration})
               (do
                 (if (:success result)
                   (do
                     (log/debug :emacsclient-success {:duration-ms duration})
                     ;; SUCCESS: recover the circuit breaker if it was half-open
                     (when (not= :closed (:state @circuit-breaker))
                       (recover-breaker!)))
                   (do
                     (log/warn :emacsclient-failure {:duration-ms duration :error (:error result)})
                     ;; IEmacsDaemon integration: detect and report daemon death
                     (when-let [[_ death-tag] (detect-daemon-death (:error result))]
                       (log/warn :daemon-death-detected {:tag death-tag :error (:error result)})
                       (report-daemon-error! (:error result) death-tag)
                       (trip-breaker! (:error result) death-tag))))
                 (assoc result :duration-ms duration))))
           (catch Exception e
             (let [duration (- (System/currentTimeMillis) start)]
               (log/error :emacsclient-exception {:duration-ms duration :exception (.getMessage e)} e)
               ;; Exception during deref — trip breaker if in half-open
               (when half-open?
                 (trip-breaker! (.getMessage e) :exception))
               {:success false
                :error (str "Exception during emacsclient call: " (.getMessage e))
                :duration-ms duration}))))))))

(defn eval-elisp
  "Execute elisp code in running Emacs and return the result.
   Returns a map with :success, :result or :error keys.
   Includes timing information for observability.
   Enforces *default-timeout-ms* (clamped to *max-timeout-ms*) automatically."
  [code]
  (eval-elisp-with-timeout code *default-timeout-ms*))

(defn eval-elisp!
  "Execute elisp and return result string, or throw on non-timeout error.
   On timeout, returns {:error :timeout :msg \"...\"}  instead of throwing,
   so callers can degrade gracefully."
  [code]
  (let [{:keys [success result error timed-out circuit-open]} (eval-elisp code)]
    (cond
      success           result
      timed-out         {:error :timeout :msg error}
      circuit-open      {:error :circuit-open :msg error}
      :else             (throw (ex-info "Elisp evaluation failed"
                                        {:error error :code code})))))

(defn emacs-running?
  "Check if Emacs server is running. Returns false on timeout."
  []
  (:success (eval-elisp-with-timeout "t" 2000)))

;; Convenience functions for common operations

(defn buffer-list
  "Get list of buffer names."
  []
  (eval-elisp! "(mapcar #'buffer-name (buffer-list))"))

(defn current-buffer
  "Get current buffer name."
  []
  (eval-elisp! "(buffer-name)"))

(defn current-file
  "Get current file path, or nil if buffer is not visiting a file."
  []
  (let [result (eval-elisp! "(buffer-file-name)")]
    (when (not= result "nil")
      result)))

(defn buffer-content
  "Get content of a buffer by name."
  [buffer-name]
  (eval-elisp! (format "(with-current-buffer \"%s\" (buffer-string))" buffer-name)))

(defn switch-to-buffer
  "Switch to a buffer by name."
  [buffer-name]
  (eval-elisp! (format "(switch-to-buffer \"%s\")" buffer-name)))

(defn find-file
  "Open a file in Emacs."
  [file-path]
  (eval-elisp! (format "(find-file \"%s\")" file-path)))

(defn save-buffer
  "Save the current buffer."
  []
  (eval-elisp! "(save-buffer)"))

(defn goto-line
  "Go to a specific line number."
  [line-number]
  (eval-elisp! (format "(goto-line %d)" line-number)))

(defn insert-text
  "Insert text at point."
  [text]
  (eval-elisp! (format "(insert \"%s\")" (str/escape text {\" "\\\"" \\ "\\\\"}))))

(defn project-root
  "Get the current project root."
  []
  (let [result (eval-elisp! "(project-root (project-current))")]
    (when (not= result "nil")
      result)))

(defn recent-files
  "Get list of recent files."
  []
  (eval-elisp! "recentf-list"))

(comment
  ;; Test functions
  (emacs-running?)
  (buffer-list)
  (current-buffer)
  (current-file)
  (project-root)

  ;; Circuit breaker inspection
  (circuit-breaker-state)
  (reset-circuit-breaker!))
