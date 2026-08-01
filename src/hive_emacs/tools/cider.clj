(ns hive-emacs.tools.cider
  "CIDER verb handlers for the `code` tool's :cider subtree — addon-owned.

   Every verb generates elisp against the hive-mcp-cider bridge and evaluates
   it through hive-emacs.client. The subtree is contributed to the host's
   `code` composite tool at addon init (addon wins over the core shadow);
   hive-mcp core needs no cider knowledge.

   spawn accepts the full CLI surface: extra_args, aliases, extra_deps,
   middleware — see hive-mcp-cider-nrepl-build-command."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [hive-dsl.result :as result]
            [hive-emacs.client :as ec]
            [hive-emacs.elisp :as el]
            [hive-emacs.tools.support :as tool]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;;; =============================================================================
;;; Boundary seam (DIP): the only host effect is evaluating elisp
;;; =============================================================================

(def ^:dynamic *eval-fn*
  "Elisp evaluation boundary: (f code) or (f code timeout-ms) ->
   {:success bool :result any :error any}. Tests bind a stub recording calls."
  (fn
    ([code] (ec/eval-elisp code))
    ([code timeout-ms] (ec/eval-elisp-with-timeout code timeout-ms))))

;;; =============================================================================
;;; Result plumbing (local — support/try-result does not catch, core's does)
;;; =============================================================================

(defn- elisp->result
  "Execute elisp and convert response to Result.
   {:success true :result r} -> (ok r), {:success false :error e} -> (err ...)
   Optional timeout-ms overrides the default emacsclient timeout."
  ([elisp] (elisp->result elisp nil))
  ([elisp timeout-ms]
   (let [{:keys [success result error]}
         (if timeout-ms
           (*eval-fn* elisp timeout-ms)
           (*eval-fn* elisp))]
     (if success
       (result/ok result)
       (result/err :cider/elisp-failed {:message (str error)})))))

(defn- try-result
  "Execute thunk f returning Result; catch unexpected exceptions as error Result."
  [category f]
  (try
    (f)
    (catch Exception e
      (log/error e (str (name category) " failed"))
      (result/err category {:message (.getMessage e)}))))

(defn- result->mcp
  "Convert Result to MCP response: ok -> mcp-success, err -> mcp-error."
  [r]
  (if (result/ok? r)
    (tool/mcp-success (:ok r))
    (tool/mcp-error (str "Error: " (or (:message r) (:error r))))))

(defn- handle-elisp
  "Common handler: execute elisp via try-result boundary, return MCP response."
  ([category elisp] (handle-elisp category elisp nil))
  ([category elisp timeout-ms]
   (result->mcp (try-result category #(elisp->result elisp timeout-ms)))))

(defn- session-arg
  "Normalize a session_name param for the elisp boundary: blank becomes nil."
  [session-name]
  (when-not (str/blank? session-name) session-name))

(defn- current-directory
  "The MCP request-context caller cwd, when a hive-mcp host binds one.
   Soft-resolved — nil in a bare REPL/test."
  []
  (result/rescue nil
    (when-let [f (resolve 'hive-mcp.agent.context/current-directory)]
      (f))))

;;; =============================================================================
;;; Auto-connect helpers (ported from hive-mcp.tools.cider)
;;; =============================================================================

(defn- cider-not-connected-error?
  [error]
  (and (string? error)
       (str/includes? (str/lower-case error) "cider not connected")))

(defn- list-sessions*
  "List CIDER sessions. Returns Result with session vector."
  []
  (result/let-ok [raw (elisp->result (el/require-and-call-json 'hive-mcp-cider 'hive-mcp-cider-list-sessions))]
                 (result/try-effect* :cider/parse-sessions
                                     (let [parsed (json/read-str raw :key-fn keyword)]
                                       (if (vector? parsed) parsed (vec parsed))))))

(defn- find-connected-session
  "Find a connected session. When project-dir is supplied, only matches
   sessions whose :project-dir equals it (trailing-slash normalized)."
  ([sessions] (find-connected-session sessions nil))
  ([sessions project-dir]
   (let [norm (fn [d] (some-> d str (str/replace #"/+$" "")))
         target (norm project-dir)]
     (some (fn [s]
             (when (and (= "connected" (:status s))
                        (or (nil? target)
                            (= target (norm (:project-dir s)))))
               (:name s)))
           sessions))))

(defn- spawn-session-internal
  "Auto-connect spawn: name (+ optional project-dir) only. Returns true on success."
  ([session-name] (spawn-session-internal session-name nil))
  ([session-name project-dir]
   (let [elisp (el/require-and-call-plist-json
                 'hive-mcp-cider 'hive-mcp-cider-spawn-session-from-plist
                 {:name session-name :project-dir project-dir})
         {:keys [success]} (*eval-fn* elisp)]
     success)))

(defn- wait-for-session-ready
  "Poll until the named session reports connected. Returns true/false."
  [session-name max-attempts]
  (loop [attempt 0]
    (if (>= attempt max-attempts)
      false
      (let [r (list-sessions*)]
        (if (and (result/ok? r)
                 (some (fn [s]
                         (and (= session-name (:name s))
                              (= "connected" (:status s))))
                       (:ok r)))
          true
          (do (Thread/sleep 500)
              (recur (inc attempt))))))))

(defn- spawn-and-wait*
  "Spawn a session and wait for readiness. Returns Result with session name."
  [session-name project-dir]
  (if (spawn-session-internal session-name project-dir)
    (if (wait-for-session-ready session-name 5)
      (result/ok session-name)
      (result/err :cider/session-timeout
                  {:message (str "Spawned session '" session-name "' but it didn't become ready in time")}))
    (result/err :cider/spawn-failed
                {:message (str "Failed to spawn session '" session-name "'")})))

(defn- ensure-connected*
  "Ensure CIDER is connected, auto-spawning a session if needed.
   project-dir scopes session reuse (auto-<hash> per project); nil falls back
   to the daemon's project root."
  ([] (ensure-connected* nil))
  ([project-dir]
   (log/debug "ensure-cider-connected: checking sessions" {:project-dir project-dir})
   (result/let-ok [sessions (list-sessions*)]
                  (if-let [session (find-connected-session sessions project-dir)]
                    (result/ok session)
                    (if project-dir
                      (let [auto-name (str "auto-" (Integer/toHexString
                                                    (.hashCode ^String project-dir)))]
                        (log/info "ensure-cider-connected: spawning session"
                                  {:name auto-name :project-dir project-dir})
                        (spawn-and-wait* auto-name project-dir))
                      (let [fallback-dir (result/rescue nil (ec/project-root))]
                        (log/info "ensure-cider-connected: spawning 'auto' (legacy)"
                                  {:project-dir fallback-dir})
                        (spawn-and-wait* "auto" fallback-dir)))))))

(defn- with-auto-connect*
  "Execute eval-thunk with auto-connect retry on 'not connected' errors."
  ([eval-thunk] (with-auto-connect* eval-thunk nil))
  ([eval-thunk project-dir]
   (let [r (eval-thunk)]
     (if (result/ok? r)
       r
       (if (cider-not-connected-error? (:message r))
         (result/let-ok [session (ensure-connected* project-dir)]
                        (log/info "with-auto-connect: reconnected via session" session)
                        (eval-thunk))
         r)))))

;;; =============================================================================
;;; Eval handlers
;;; =============================================================================

(defn- handle-eval-common
  "Shared eval path: directory resolution (param, then request-context cwd),
   project-scoped session routing, legacy default-connection fallback.
   mode is \"silent\" or \"explicit\"."
  [params mode]
  (let [{:keys [code timeout]} params
        directory (or (:directory params)
                      (:project_dir params)
                      (current-directory))
        nrepl-timeout (or timeout 60)
        ec-timeout-ms (+ (* nrepl-timeout 1000) 2000)
        elisp-fn (fn [code]
                   (if (= mode "explicit")
                     (el/require-and-call-text 'hive-mcp-cider 'hive-mcp-cider-eval-explicit code)
                     (el/require-and-call-text 'hive-mcp-cider 'hive-mcp-cider-eval-silent code (or timeout nil))))]
    (result->mcp
     (try-result :cider/eval-failed
                 (fn []
                   (binding [ec/*max-timeout-ms* (max ec/*max-timeout-ms* ec-timeout-ms)]
                     (if directory
                       (result/let-ok [session (ensure-connected* directory)]
                                      (elisp->result
                                        (el/require-and-call-text
                                          'hive-mcp-cider 'hive-mcp-cider-eval-in-session
                                          session code (or timeout nil))
                                        ec-timeout-ms))
                       (with-auto-connect*
                         (fn [] (elisp->result (elisp-fn code) ec-timeout-ms))
                         nil))))))))

(defn handle-eval
  "Evaluate Clojure code via CIDER. mode selects silent (default) or explicit;
   session_name routes to a named session instead."
  [{:keys [mode session_name] :as params}]
  (if (session-arg session_name)
    (let [{:keys [code timeout]} params
          ec-timeout-ms (+ (* (or timeout 60) 1000) 2000)]
      (result->mcp
       (try-result :cider/eval-failed
                   (fn []
                     (binding [ec/*max-timeout-ms* (max ec/*max-timeout-ms* ec-timeout-ms)]
                       (elisp->result
                         (el/require-and-call-text
                           'hive-mcp-cider 'hive-mcp-cider-eval-in-session
                           session_name code (or timeout nil))
                         ec-timeout-ms))))))
    (handle-eval-common params (or mode "silent"))))

;;; =============================================================================
;;; Introspection handlers
;;; =============================================================================

(defn handle-status
  "Get CIDER connection status."
  [_]
  (handle-elisp :cider/status-failed
                (el/require-and-call-json 'hive-mcp-cider 'hive-mcp-cider-status)))

(defn handle-doc
  "Docstring for a symbol, optionally inside SESSION_NAME's REPL."
  [{:keys [symbol session_name]}]
  (handle-elisp :cider/doc-failed
                (el/require-and-call-json 'hive-mcp-cider 'hive-mcp-cider-doc
                                          symbol (session-arg session_name))))

(defn handle-info
  "Full semantic info for a symbol, optionally inside SESSION_NAME's REPL."
  [{:keys [symbol session_name]}]
  (handle-elisp :cider/info-failed
                (el/require-and-call-json 'hive-mcp-cider 'hive-mcp-cider-info
                                          symbol (session-arg session_name))))

(defn handle-complete
  "Completions for a prefix, optionally inside SESSION_NAME's REPL."
  [{:keys [prefix session_name]}]
  (handle-elisp :cider/complete-failed
                (el/require-and-call-json 'hive-mcp-cider 'hive-mcp-cider-complete
                                          prefix (session-arg session_name))))

(defn handle-apropos
  "Search symbols matching a pattern, optionally inside SESSION_NAME's REPL."
  [{:keys [pattern search_docs session_name]}]
  (handle-elisp :cider/apropos-failed
                (el/require-and-call-json 'hive-mcp-cider 'hive-mcp-cider-apropos
                                          pattern (boolean search_docs)
                                          (session-arg session_name))))

;;; =============================================================================
;;; Session lifecycle handlers
;;; =============================================================================

(def spawn-schema-params
  "The addon-contributed `code` schema params for the full spawn CLI surface."
  {"extra_args" {:type "array"
                 :items {:type "string"}
                 :description "spawn only: raw clojure CLI args spliced after -Sdeps and before the -M flag (e.g. [\"-Srepro\"] or JVM opts like [\"-J-Xmx4g\"]). A raw -Sdeps here REPLACES the merged one — use extra_deps to layer instead. Everything after -M is main-opts, so CLI opts must go here."}
   "aliases" {:type "array"
              :items {:type "string"}
              :description "spawn only: bare CLI alias names selecting the -M flag for this spawn (e.g. [\"test\"] -> -M:test). Overrides the daemon-wide launch-aliases defcustom."}
   "extra_deps" {:type "array"
                 :items {:type "string"}
                 :description "spawn only: deps EDN maps (as strings) deep-merged into the built-in -Sdeps, after auto-detected local.deps.edn — later entries win key-wise. This is the -Sdeps \"$(cat file)\" equivalent: pass the file's CONTENTS, not a shell form."}
   "middleware" {:type "array"
                 :items {:type "string"}
                 :description "spawn only: nREPL middleware symbol strings appended to the built-in list (e.g. [\"refactor-nrepl.middleware/wrap-refactor\"])."}})

(defn handle-spawn
  "Spawn a new named CIDER session with its own nREPL server.
   Full CLI surface: extra_args (raw, pre--M), aliases (-M selection),
   extra_deps (EDN strings merged into -Sdeps), middleware (appended).
   local.deps.edn in the project dir is always auto-detected."
  [{:keys [name project_dir agent_id repl_type port extra_args aliases extra_deps middleware]}]
  (log/info "cider-spawn" {:name name :repl_type repl_type :agent_id agent_id :port port})
  (if (str/blank? name)
    (tool/mcp-error "Error: spawn requires a non-blank 'name'")
    (let [port (cond-> port (string? port) parse-long)]
      (handle-elisp :cider/spawn-failed
                    (el/require-and-call-plist-json
                      'hive-mcp-cider 'hive-mcp-cider-spawn-session-from-plist
                      {:name       name
                       :repl-type  (when repl_type (symbol repl_type))
                       :port       port
                       :project-dir project_dir
                       :agent-id   agent_id
                       :extra-args (when extra_args (vec extra_args))
                       :aliases    (when aliases (vec aliases))
                       :extra-deps (when extra_deps (vec extra_deps))
                       :middleware (when middleware (vec middleware))})))))

(defn handle-connect
  "Connect to an existing nREPL server as a named session.
   project_dir labels the REPL buffer for cross-project sessions."
  [{:keys [name host port repl_type agent_id project_dir]}]
  (if (or (str/blank? name) (nil? port))
    (tool/mcp-error "Error: connect requires 'name' and 'port'")
    (let [port (cond-> port (string? port) parse-long)]
      (result->mcp
       (try-result :cider/connect-failed
                   (fn []
                     (let [elisp (el/require-and-call-json
                                   'hive-mcp-cider 'hive-mcp-cider-connect-session
                                   name (or host "localhost") port
                                   (or repl_type "clj") agent_id project_dir)
                           {:keys [success result error]} (*eval-fn* elisp 20000)]
                       (if success
                         (result/ok result)
                         (result/err :cider/elisp-failed {:message (str error)})))))))))

(defn handle-sessions
  "List all active CIDER sessions with their status and ports."
  [_]
  (handle-elisp :cider/list-sessions-failed
                (el/require-and-call-json 'hive-mcp-cider 'hive-mcp-cider-list-sessions)))

(defn handle-kill-session
  "Kill a specific named session. Blank name errors; elisp errors surface."
  [{:keys [session_name]}]
  (if (str/blank? session_name)
    (tool/mcp-error "Error: kill-session requires a non-blank 'session_name'")
    (result->mcp
     (try-result :cider/kill-session-failed
                 (fn []
                   (elisp->result
                     (el/require-and-call 'hive-mcp-cider 'hive-mcp-cider-kill-session session_name)))))))

(defn handle-kill-all
  "Kill all CIDER sessions."
  [_]
  (result->mcp
   (try-result :cider/kill-all-failed
               (fn []
                 (elisp->result
                   (el/require-and-call 'hive-mcp-cider 'hive-mcp-cider-kill-all-sessions))))))

;;; =============================================================================
;;; Subtree + contribution
;;; =============================================================================

(def handlers
  "The addon-owned :cider verb tree for the `code` composite tool."
  {:eval          handle-eval
   :doc           handle-doc
   :info          handle-info
   :complete      handle-complete
   :apropos       handle-apropos
   :status        handle-status
   :spawn         handle-spawn
   :connect       handle-connect
   :sessions      handle-sessions
   :kill-session  handle-kill-session
   :kill-all      handle-kill-all
   ;; deprecated aliases (core parity)
   :eval-explicit (fn [params] (handle-eval (assoc params :mode "explicit")))
   :eval-session  handle-eval})

(defn- subdomain-handler
  "Strip the \"<subdomain> \" prefix off :command before calling INNER."
  [subdomain-name inner]
  (fn [params]
    (let [full   (str (:command params))
          prefix (str subdomain-name " ")]
      (inner (assoc params :command (if (str/starts-with? full prefix)
                                      (subs full (count prefix))
                                      full))))))

(def handle-cider-subdomain
  "Entry point contributed to `code` under \"cider\": strips the prefix and
   dispatches into the addon's verb tree."
  (subdomain-handler "cider" (tool/make-cli-handler handlers)))

(def commands
  "Command contribution map for hive-mcp.extensions.registry/contribute-commands!."
  {"cider" {:handler     handle-cider-subdomain
            :params      spawn-schema-params
            :description "CIDER REPL operations (hive.emacs addon): eval (silent|explicit), doc, info, complete, apropos, status, spawn (extra_args/aliases/extra_deps/middleware; local.deps.edn auto-detected), connect, sessions, kill-session, kill-all."}})

(defn contribute!
  "Register the :cider subtree into the host's `code` composite tool.
   Soft-resolved — no-op outside a live hive-mcp host. Idempotent."
  []
  (when-let [contribute (resolve 'hive-mcp.extensions.registry/contribute-commands!)]
    (contribute "code" "hive.emacs" commands)
    (log/info "hive-emacs: contributed :cider subtree to `code`"
              {:verbs (keys handlers)})))

(defn retract!
  "Retract the addon's `code` contributions. No-op outside a live host."
  []
  (when-let [retract (resolve 'hive-mcp.extensions.registry/retract-commands!)]
    (retract "code" "hive.emacs")))
