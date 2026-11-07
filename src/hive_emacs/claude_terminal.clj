(ns hive-emacs.claude-terminal
  "The :claude terminal: a Claude Code session inside an Emacs buffer
   (claude-code-ide by default, or vterm / eat, as configured in Emacs).

   hive-claude is vessel-neutral and ships only the headless backend; the
   Emacs half of Claude Code lives here, beside the hive-claude-* elisp
   package in elisp/. The addon registers this terminal with the host's
   terminal registry at init, by soft resolution, and removes it at shutdown.

   Loading is reconnect-aware: the hive-claude features are (re)required
   whenever the Emacs session fingerprint changes, so a restarted Emacs gets
   them back without a JVM restart."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [hive-addon.terminal :as addon-term]
            [hive-emacs.bridge-loader :as bridge]
            [hive-emacs.client :as ec]
            [hive-spi.editor.elisp :as el]
            [taoensso.timbre :as log]))

;; SPDX-License-Identifier: MIT

(def terminal-key
  "The terminal id this ns registers, unchanged from hive-claude so live
   lings keep resolving."
  :claude)

;; =============================================================================
;; Loading the hive-claude elisp, reconnect-aware
;; =============================================================================

(def ^:private features
  "Required in dependency order: config, state, bridge, sync."
  ["hive-claude-config" "hive-claude-state" "hive-claude-bridge" "hive-claude-sync"])

(defonce ^:private loaded-fp (atom nil))

(defn- unquote-string
  "Strip one pair of surrounding double quotes, if present."
  [s]
  (cond-> s
    (str/starts-with? s "\"") (subs 1)
    (and (str/ends-with? s "\"") (pos? (count s))) (as-> t (subs t 0 (dec (count t))))))

(defn- session-fingerprint
  "(emacs-pid)|(emacs-init-time): differs for every Emacs process."
  [eval-fn]
  (let [{:keys [success result]}
        (eval-fn "(format \"%s|%s\" (emacs-pid) (format-time-string \"%s.%N\" (or (emacs-init-time) (current-time))))"
                 2000)]
    (when (and success (string? result) (seq result))
      (unquote-string result))))

(defn- features-present?
  [eval-fn]
  (let [{:keys [success result]} (eval-fn "(if (featurep 'hive-claude-bridge) \"t\" \"nil\")" 2000)]
    (boolean (and success (string? result) (= "t" (unquote-string (str/trim result)))))))

(defn reset-load-state!
  "Forget the loaded session; the next call reloads the features."
  []
  (reset! loaded-fp nil))

(defn ensure-loaded!
  "Ensure the hive-claude features are loaded in the live Emacs session.
   True when they are. Never throws."
  [eval-fn]
  (try
    (let [fp (session-fingerprint eval-fn)]
      (if (and fp (= fp @loaded-fp) (features-present? eval-fn))
        true
        (let [ok? (and (bridge/ensure-loaded-once! eval-fn)
                       (:success (eval-fn (format "(progn %s t)"
                                                  (str/join " " (map #(format "(require '%s)" %) features)))
                                          5000)))]
          (if ok?
            (do (when-not (:success (eval-fn "(hive-claude-sync-register-hooks-bang)" 3000))
                  (log/warn "hive-emacs: hive-claude sync hooks did not register"))
                (reset! loaded-fp fp)
                true)
            (do (log/warn "hive-emacs: hive-claude elisp did not load")
                false)))))
    (catch Exception e
      (log/warn "hive-emacs: hive-claude elisp load failed" {:error (ex-message e)})
      false)))

;; =============================================================================
;; Bridge calls
;; =============================================================================

(defn- call
  "Evaluate `elisp` (which JSON-encodes its answer) through EVAL-FN.
   {:success true :result parsed} or {:success false :error str}."
  [eval-fn elisp timeout-ms]
  (let [{:keys [success result error timed-out]} (eval-fn elisp timeout-ms)]
    (cond
      timed-out {:success false :error "timeout" :timed-out true}
      success   {:success true
                 :result  (if (string? result)
                            (try (json/read-str result :key-fn keyword)
                                 (catch Exception _ result))
                            result)}
      :else     {:success false :error (str error)})))

(def ^:private ^:const max-context-chars
  "Claude CLI crashes when the system prompt exceeds ~120KB; context is
   capped at 80KB to leave room for presets and identity."
  80000)

(defn- spawn-context-file
  "Write the host's catchup context for `cwd` (from SPAWN-CONTEXT, the
   :catchup/spawn-context port) to a temp file; its path, or nil."
  [spawn-context cwd]
  (try
    (when-let [ctx (and spawn-context (spawn-context cwd))]
      (let [ctx (if (> (count ctx) max-context-chars)
                  (do (log/warn "Spawn context truncated" {:original-size (count ctx) :max max-context-chars})
                      (subs ctx 0 max-context-chars))
                  ctx)
            tmp (java.io.File/createTempFile "hive-claude-ctx-" ".md")]
        (spit tmp ctx)
        (.getAbsolutePath tmp)))
    (catch Exception e
      (log/debug "hive-emacs: no spawn context" {:error (ex-message e)})
      nil)))

(defn- q [v] (el/elisp-quote v))

(defn spawn-elisp
  "The elisp that spawns ling `id` (pure)."
  [id name presets cwd ctx-file]
  (format "(json-encode (hive-claude-bridge-api-spawn %s %s %s %s 1 nil %s))"
          (q (str id)) (q (str (or name id)))
          (if (seq presets) (str "'(" (str/join " " (map (comp q str) presets)) ")") "nil")
          (if cwd (q (str cwd)) "nil")
          (if ctx-file (q ctx-file) "nil")))

(def default-opts
  "Timing and size limits of the :claude terminal. :max-task-chars bounds a
   dispatched task; :spawn-polls x :spawn-poll-ms is how long a spawn waits
   for a live terminal process; :dispatch-poll-ms paces the reads of the
   paste outcome, whose count is derived from the task size unless
   :dispatch-polls is given."
  {:max-task-chars   max-context-chars
   :spawn-polls      20
   :spawn-poll-ms    250
   :dispatch-poll-ms 250
   :sleep-fn         (fn [ms] (Thread/sleep (long ms)))})

(defn- dispatch-polls
  "Polls that cover pasting `chars` in 512-char steps 50 ms apart, the
   bridge's 5 s settle ceiling, and one second of slack."
  [{:keys [dispatch-polls dispatch-poll-ms]} chars]
  (or dispatch-polls
      (long (Math/ceil (/ (+ (* 50.0 (Math/ceil (/ chars 512.0))) 6000.0)
                          dispatch-poll-ms)))))

(defn- poll
  "Call `probe` up to `n` times, sleeping `ms` between calls, until `done?`
   holds for its answer. The last answer."
  [{:keys [sleep-fn]} n ms probe done?]
  (loop [i 1]
    (let [a (probe)]
      (if (or (done? a) (>= i n))
        a
        (do (sleep-fn ms) (recur (inc i)))))))

(defn- terminal-alive?
  [{:keys [buffer-alive process-alive]}]
  (boolean (and buffer-alive process-alive)))

(defn- await-live-terminal!
  "Wait until ling `id`'s terminal buffer and process are alive. Throws,
   after killing the half-made session, when they never are."
  [eval-fn opts id]
  (let [answer (poll opts (:spawn-polls opts) (:spawn-poll-ms opts)
                     #(call eval-fn (format "(json-encode (hive-claude-bridge-api-alive %s))" (q (str id))) 3000)
                     #(terminal-alive? (:result %)))
        seen   (when (:success answer) (:result answer))]
    (when-not (terminal-alive? seen)
      (try (call eval-fn (format "(json-encode (hive-claude-bridge-api-kill %s))" (q (str id))) 5000)
           (catch Exception e
             (log/debug "hive-emacs: dead claude session not killed" {:id id :error (ex-message e)})))
      (throw (ex-info (str "Claude terminal spawn failed: the terminal process for " id
                           " is not running after the spawn"
                           (when-not (:success answer) (str " (" (:error answer) ")")))
                      {:id id :reason :terminal-not-alive
                       :buffer-alive (boolean (:buffer-alive seen))
                       :process-alive (boolean (:process-alive seen))
                       :error (:error answer)})))))

(defn- refused [reason m] (merge {:dispatched false :reason reason} m))

(defn- dispatch-task!
  "Paste `task` into ling `id` and wait for the bridge to report it
   delivered. true, or a {:dispatched false :reason ...} refusal."
  [eval-fn opts id task]
  (let [task  (str task)
        chars (count task)
        max   (:max-task-chars opts)]
    (if (> chars max)
      (refused :task-too-large
               {:chars chars :max-chars max
                :error (str "task of " chars " chars exceeds the " max
                            "-char limit of the :claude terminal; put the brief in a hive memory and dispatch a pointer to it")})
      (let [{:keys [success result error]}
            (call eval-fn (format "(json-encode (hive-claude-bridge-api-dispatch %s %s))" (q (str id)) (q task)) 5000)]
        (cond
          (not success)                     (throw (ex-info "Claude terminal dispatch failed" {:ling-id id :error error}))
          (not (and (map? result) (:sent result))) (refused :no-live-session {:ling-id id})
          :else
          (let [answer (poll opts (dispatch-polls opts chars) (:dispatch-poll-ms opts)
                             #(call eval-fn (format "(json-encode (hive-claude-bridge-api-status %s))" (q (str id))) 3000)
                             #(contains? #{"delivered" "failed"} (get-in % [:result :paste])))
                state  (get-in answer [:result :paste])]
            (case state
              "delivered" true
              "failed"    (refused :paste-failed {:ling-id id :chars chars})
              (refused :paste-timeout {:ling-id id :chars chars :paste state}))))))))

(defn make-claude-terminal
  "An ITerminalAddon for Claude Code sessions in Emacs, over EVAL-FN
   ((f code timeout-ms) -> {:success bool :result str ...}). SPAWN-CONTEXT is
   the host's :catchup/spawn-context port (fn [cwd] -> markdown), or nil.
   OPTS overrides `default-opts`.

   A spawn returns only once the terminal process runs, and throws
   otherwise. A dispatch returns true only once the bridge reports the whole
   task pasted and submitted; otherwise it answers a {:dispatched false}
   refusal."
  ([] (make-claude-terminal ec/eval-elisp-with-timeout nil))
  ([eval-fn] (make-claude-terminal eval-fn nil))
  ([eval-fn spawn-context] (make-claude-terminal eval-fn spawn-context nil))
  ([eval-fn spawn-context opts]
   (let [opts (merge default-opts opts)]
     (reify addon-term/ITerminalAddon
       (terminal-id [_] terminal-key)

       (terminal-spawn! [_ ctx opts']
         (let [{:keys [id cwd presets]} ctx]
           (when-not (ensure-loaded! eval-fn)
             (throw (ex-info "Claude terminal spawn failed: the hive-claude elisp did not load in Emacs"
                             {:id id :reason :bridge-not-loaded})))
           (let [ctx-file (when (:task opts') (spawn-context-file spawn-context cwd))
                 {:keys [success error]} (call eval-fn (spawn-elisp id (:name opts') presets cwd ctx-file) 10000)]
             (when-not success
               (throw (ex-info "Claude terminal spawn failed" {:id id :error error})))
             (await-live-terminal! eval-fn opts id)
             (log/info "Claude terminal spawned ling" {:id id :cwd cwd})
             id)))

       (terminal-dispatch! [_ ctx task-opts]
         (ensure-loaded! eval-fn)
         (let [{:keys [id]} ctx
               answer (dispatch-task! eval-fn opts id (:task task-opts))]
           (if (true? answer)
             (log/info "Claude terminal dispatched task" {:id id})
             (log/warn "Claude terminal refused dispatch" (assoc answer :id id)))
           answer))

       (terminal-status [_ ctx ds-status]
         (ensure-loaded! eval-fn)
         (let [{:keys [id]} ctx
               {:keys [success result]} (call eval-fn
                                              (format "(json-encode (hive-claude-bridge-api-status %s))" (q (str id)))
                                              3000)]
           (if success
             (merge (or ds-status {})
                    {:slave/id        id
                     :elisp-alive?    (get result :buffer-alive false)
                     :terminal-status (get result :status)})
             ds-status)))

       (terminal-kill! [_ ctx]
         (ensure-loaded! eval-fn)
         (let [{:keys [id]} ctx
               {:keys [success result error]} (call eval-fn
                                                    (format "(json-encode (hive-claude-bridge-api-kill %s))" (q (str id)))
                                                    5000)]
           (cond
             (not success)     {:killed? false :id id :reason :elisp-error :error error}
             (:killed result)  {:killed? true :id id}
             :else             {:killed? false :id id :reason :kill-blocked})))

       (terminal-interrupt! [_ ctx]
         (ensure-loaded! eval-fn)
         (let [{:keys [id]} ctx
               {:keys [success error]} (call eval-fn
                                             (format "(json-encode (hive-claude-bridge-api-interrupt %s))" (q (str id)))
                                             3000)]
           (if success
             {:success? true :ling-id id}
             {:success? false :ling-id id :errors [(str "interrupt failed: " error)]})))))))

;; =============================================================================
;; Registration with the host
;; =============================================================================

(defn register!
  "Register the :claude terminal through the host's :terminal/register!
   runtime port. Returns true when registered, false when the host offers no
   such port."
  ([ports] (register! ports ec/eval-elisp-with-timeout))
  ([ports eval-fn]
   (if-let [reg (:terminal/register! ports)]
     (try
       (boolean (:registered? (reg terminal-key (make-claude-terminal eval-fn (:catchup/spawn-context ports)))))
       (catch Exception e
         (log/warn "hive-emacs: :claude terminal registration failed" {:error (ex-message e)})
         false))
     false)))

(defn unregister!
  "Drop the :claude terminal (through the host's :terminal/deregister!
   port) and the Emacs-side sync hooks. Never throws."
  ([ports] (unregister! ports ec/eval-elisp-with-timeout))
  ([ports eval-fn]
   (try (eval-fn "(when (fboundp 'hive-claude-sync-unregister-hooks-bang) (hive-claude-sync-unregister-hooks-bang))" 3000)
        (catch Exception e
          (log/debug "hive-emacs: sync hooks not unregistered" {:error (ex-message e)})))
   (when-let [dereg (:terminal/deregister! ports)]
     (try (dereg terminal-key)
          (catch Exception e
            (log/warn "hive-emacs: :claude terminal deregistration failed" {:error (ex-message e)}))))
   (reset-load-state!)
   nil))
