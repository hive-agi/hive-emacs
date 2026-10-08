(ns hive-emacs.tools.cider-test
  "Unit tests for hive-emacs.tools.cider — the addon-owned :cider verb tree.

   DIP-in-tests: the ONLY host effect is the elisp-eval boundary, injected via
   cider/*eval-fn*. The stub records every elisp form and answers canned
   responses per call — no Emacs, no hive-mcp, no nREPL."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-emacs.tools.cider :as cider]
            [hive-emacs.cider.spawn :as spawn]
            [clojure.data.json :as json]
            [hive-emacs.cider.spawn-dir :as spawn-dir]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;;; =============================================================================
;;; Boundary stub
;;; =============================================================================

(defn- make-stub
  "A stub *eval-fn*: RESPONDER maps an elisp string to a response map
   {:success :result/:error}; every call is recorded in the calls atom."
  [responder]
  (let [calls (atom [])]
    {:calls calls
     :eval-fn (fn
                ([code]
                 (swap! calls conj code)
                 (responder code))
                ([code _timeout-ms]
                 (swap! calls conj code)
                 (responder code)))}))

(defn- ok-stub []
  (make-stub (fn [_] {:success true :result "\"{}\""})))

;;; =============================================================================
;;; spawn — the full CLI surface reaches the plist boundary
;;; =============================================================================

(deftest spawn-forwards-full-cli-surface
  (let [{:keys [calls eval-fn]} (ok-stub)]
    (binding [cider/*eval-fn* eval-fn
              spawn-dir/*directory?* #{"/p"}]
      (cider/handle-spawn {:name "dev"
                           :project_dir "/p"
                           :repl_type "clj"
                           :port 7999
                           :extra_args ["-Srepro"]
                           :aliases ["test"]
                           :extra_deps ["{:deps {my/lib {:local/root \"../lib\"}}}"]
                           :middleware ["refactor-nrepl.middleware/wrap-refactor"]})
      (let [form (first @calls)]
        (is (= 1 (count @calls)))
        (is (str/includes? form ":name \"dev\""))
        (is (str/includes? form ":project-dir \"/p\""))
        (is (str/includes? form ":repl-type 'clj"))
        (is (str/includes? form ":port 7999"))
        (is (str/includes? form ":extra-args '(\"-Srepro\")"))
        (is (str/includes? form ":aliases '(\"test\")"))
        (is (str/includes? form ":extra-deps '("))
        (is (str/includes? form "my/lib"))
        (is (str/includes? form ":middleware '(\"refactor-nrepl.middleware/wrap-refactor\")"))))))

(deftest spawn-timeout-is-watched-not-abandoned
  (let [{:keys [eval-fn]} (make-stub (fn [_] {:success false
                                              :error "Emacsclient call timed out after 5000ms"
                                              :timed-out true}))]
    (spawn/reset-watches!)
    (try
      (binding [cider/*eval-fn* eval-fn
                cider/*attention-fn* (constantly nil) spawn-dir/*directory?* #{"/p"}]
        (let [response (cider/handle-spawn {:name "slow" :project_dir "/p" :port 7990})
              text (str (:text response) (get-in response [:content 0 :text]))]
          (is (:isError response))
          (is (str/includes? text "timed out"))
          (is (str/includes? text "---CIDER-SPAWN---"))
          (is (str/includes? text "Do not respawn"))
          (is (= {:name "slow" :port 7990 :repl-type "clj" :project-dir "/p"}
                 (select-keys (get (spawn/watches) "slow")
                              [:name :port :repl-type :project-dir])))))
      (finally (spawn/reset-watches!)))))

(deftest spawn-hard-failure-is-not-watched
  (let [{:keys [eval-fn]} (make-stub (fn [_] {:success false :error "void-function"}))]
    (spawn/reset-watches!)
    (try
      (binding [cider/*eval-fn* eval-fn
                cider/*attention-fn* (constantly nil)]
        (let [response (cider/handle-spawn {:name "broken" :project_dir "/p"})]
          (is (:isError response))
          (is (not (str/includes? (pr-str response) "Do not respawn")))
          (is (empty? (spawn/watches)))))
      (finally (spawn/reset-watches!)))))

(deftest spawn-omits-nil-params
  (let [{:keys [calls eval-fn]} (ok-stub)]
    (binding [cider/*eval-fn* eval-fn]
      (cider/handle-spawn {:name "bare"})
      (let [form (first @calls)]
        (is (str/includes? form ":name \"bare\""))
        (is (not (str/includes? form ":extra-args")))
        (is (not (str/includes? form ":aliases")))
        (is (not (str/includes? form ":extra-deps")))
        (is (not (str/includes? form ":middleware")))))))

(deftest spawn-rejects-blank-name
  (binding [cider/*eval-fn* (:eval-fn (ok-stub))]
    (let [resp (cider/handle-spawn {:name ""})]
      (is (true? (:isError resp)))
      (is (str/includes? (:text resp) "name")))))

(deftest spawn-coerces-string-port
  (let [{:keys [calls eval-fn]} (ok-stub)]
    (binding [cider/*eval-fn* eval-fn]
      (cider/handle-spawn {:name "p" :port "7999"})
      (is (str/includes? (first @calls) ":port 7999"))
      (is (not (str/includes? (first @calls) ":port \"7999\""))))))

(deftest spawn-accepts-aliases-as-a-string
  (doseq [given ["test" ":test" "dev,test" ":dev:test"]]
    (let [{:keys [calls eval-fn]} (ok-stub)]
      (binding [cider/*eval-fn* eval-fn]
        (cider/handle-spawn {:name "s" :aliases given})
        (let [form (first @calls)]
          (is (re-find #":aliases '\((\"dev\" )?\"test\"\)" form) (str given " -> " form))
          (is (not (str/includes? form "?t")) "never split into characters"))))))

;;; =============================================================================
;;; kill-session — fail loud on blank (no silent no-op)
;;; =============================================================================

(deftest kill-session-rejects-blank
  (let [{:keys [calls eval-fn]} (ok-stub)]
    (binding [cider/*eval-fn* eval-fn]
      (let [resp (cider/handle-kill-session {:session_name nil})]
        (is (true? (:isError resp))))
      (is (empty? @calls)))))

(deftest kill-session-unknown-surfaces-elisp-error
  (let [{:keys [eval-fn]}
        (make-stub (fn [_] {:success false
                            :error "hive-mcp-cider: unknown session 'ghost'"}))]
    (binding [cider/*eval-fn* eval-fn]
      (let [resp (cider/handle-kill-session {:session_name "ghost"})]
        (is (true? (:isError resp)))
        (is (str/includes? (:text resp) "unknown session"))))))

;;; =============================================================================
;;; eval — session routing + auto-connect spawn
;;; =============================================================================

(defn- session-entry
  "A registry row: a bare name is a connected clj session whose REPL buffer is
   \"*repl NAME*\"; a map overrides any of those fields."
  [s]
  (merge {:status "connected" :repl-type "clj"}
         (if (string? s) {:name s :cider-buffer (str "*repl " s "*")} s)))

(def ^:private ok-info-envelope
  "{\"ok\":{\"name\":\"map\",\"ns\":\"clojure.core\",\"doc\":\"d\",\"arglists-str\":\"[f coll]\",\"status\":[\"done\"]}}")

(defn- bridge-session
  "The session argument of the bridge introspection call in CODE: the last
   argument when it is a string, else :default."
  [code]
  (let [args (second (re-find #"\(hive-mcp-cider-(?:doc|info|complete|apropos) ([^()]*)\)" code))]
    (or (second (re-find #"\"([^\"]+)\"\s*$" (str args))) :default)))

(defn- registry-stub
  "A stub *eval-fn* standing in for Emacs + nREPL: SESSIONS is the registry
   listing (names or registry rows). Each eval-in-session call is recorded in
   :received as the session it was addressed to; an eval NOT addressed to a
   named session as :default. A bounded nREPL request is recorded as
   [:nrepl buffer-or-:current] and answered by NREPL (code -> envelope JSON);
   a bridge introspection call as [:bridge session-or-:default]."
  ([sessions] (registry-stub sessions (constantly ok-info-envelope)))
  ([sessions nrepl]
   (let [received (atom [])
         calls (atom [])
         sessions-json (str "[" (str/join "," (map (fn [s]
                                                      (let [{:keys [name status repl-type cider-buffer]} (session-entry s)]
                                                        (str "{\"name\":\"" name "\",\"status\":\"" status
                                                             "\",\"repl-type\":\"" repl-type "\""
                                                             (when cider-buffer (str ",\"cider-buffer\":\"" cider-buffer "\""))
                                                             "}")))
                                                    sessions))
                            "]")
         respond (fn [code]
                   (swap! calls conj code)
                   (cond
                     (str/includes? code "hive-mcp-cider-list-sessions")
                     {:success true :result sessions-json}

                     (str/includes? code "cider-nrepl-send-request")
                     (do (swap! received conj [:nrepl (or (second (re-find #"\(get-buffer \"([^\"]+)\"\)" code))
                                                          :current)])
                         {:success true :result (nrepl code)})

                     (str/includes? code "hive-mcp-cider-eval-in-session")
                     (let [target (second (re-find #"hive-mcp-cider-eval-in-session\s+\"([^\"]+)\"" code))]
                       (swap! received conj target)
                       {:success true :result (str "ran-in:" target)})

                     (re-find #"hive-mcp-cider-(eval-silent|eval-explicit)" code)
                     (do (swap! received conj :default)
                         {:success true :result "ran-in:default"})

                     (re-find #"hive-mcp-cider-(doc|info|complete|apropos)" code)
                     (do (swap! received conj [:bridge (bridge-session code)])
                         {:success true :result "{\"bridge\":true}"})

                     :else {:success true :result "\"{}\""}))]
     {:received received
      :calls calls
      :eval-fn (fn ([code] (respond code)) ([code _timeout-ms] (respond code)))})))

(deftest eval-routes-to-named-session
  (let [{:keys [received eval-fn]} (registry-stub ["coord" "s1"])]
    (binding [cider/*eval-fn* eval-fn]
      (let [resp (cider/handle-eval {:code "(+ 1 2)" :session_name "s1"})]
        (is (not (:isError resp)))
        (is (= "ran-in:s1" (:text resp))))
      (is (= ["s1"] @received) "the eval reached s1's connection and no other"))))

(deftest eval-session-verb-routes-the-name-param
  (let [{:keys [received eval-fn]} (registry-stub ["coord" "spawned"])
        handler (get cider/handlers :eval-session)]
    (binding [cider/*eval-fn* eval-fn]
      (is (= "ran-in:spawned" (:text (handler {:code "(System/getProperty \"user.dir\")"
                                              :name "spawned"}))))
      (is (= "ran-in:spawned" (:text (handler {:code "1" :session_name "spawned"}))))
      (is (= ["spawned" "spawned"] @received)))))

(deftest eval-session-verb-without-a-name-is-refused
  (let [{:keys [calls eval-fn]} (registry-stub ["coord"])]
    (binding [cider/*eval-fn* eval-fn]
      (let [resp ((get cider/handlers :eval-session) {:code "1"})]
        (is (true? (:isError resp)))
        (is (str/includes? (:text resp) "session_name")))
      (is (empty? @calls) "nothing is evaluated anywhere"))))

(deftest unknown-session-name-is-refused-never-defaulted
  (let [{:keys [received eval-fn]} (registry-stub ["coord" "alpha"])
        eval-session (get cider/handlers :eval-session)]
    (binding [cider/*eval-fn* eval-fn]
      (doseq [resp [(cider/handle-eval {:code "1" :session_name "ghost"})
                    (eval-session {:code "1" :name "ghost"})
                    (cider/handle-doc {:symbol "map" :session_name "ghost"})
                    (cider/handle-info {:symbol "map" :session_name "ghost"})
                    (cider/handle-complete {:prefix "ma" :session_name "ghost"})
                    (cider/handle-apropos {:pattern "ma" :session_name "ghost"})]]
        (is (true? (:isError resp)))
        (is (str/includes? (:text resp) "Unknown CIDER session 'ghost'"))
        (is (str/includes? (:text resp) "Known sessions: alpha, coord")))
      (is (empty? @received) "no connection received anything"))))

(deftest named-introspection-reaches-the-named-session
  (let [{:keys [received calls eval-fn]} (registry-stub ["coord" "s1"])]
    (binding [cider/*eval-fn* eval-fn]
      (doseq [resp [(cider/handle-doc {:symbol "map" :session_name "s1"})
                    (cider/handle-info {:symbol "map" :session_name "s1"})
                    (cider/handle-complete {:prefix "ma" :session_name "s1"})
                    (cider/handle-apropos {:pattern "ma" :session_name "s1"})]]
        (is (not (:isError resp)) (pr-str resp)))
      (is (= (repeat 4 [:nrepl "*repl s1*"]) @received)
          "each verb went to s1's REPL buffer over the bounded boundary, nowhere else")
      (is (every? #(str/includes? % "with-timeout")
                  (filter #(str/includes? % "cider-nrepl-send-request") @calls))))))

(deftest introspection-keeps-its-json-shape
  (let [{:keys [eval-fn]} (registry-stub ["s1"])]
    (binding [cider/*eval-fn* eval-fn]
      (is (= {"doc" "d" "arglists" "[f coll]" "ns" "clojure.core" "name" "map" "file" "" "line" 0}
             (json/read-str (:text (cider/handle-doc {:symbol "map" :session_name "s1"})))))
      (is (= {"name" "map" "ns" "clojure.core" "doc" "d" "arglists" "[f coll]"}
             (json/read-str (:text (cider/handle-info {:symbol "map" :session_name "s1"}))))))))

(deftest a-cljel-session-is-introspected-by-the-bridge
  (let [{:keys [received eval-fn]}
        (registry-stub [{:name "el" :repl-type "cljel" :cider-buffer "*repl el*"}])]
    (binding [cider/*eval-fn* eval-fn]
      (is (= "{\"bridge\":true}" (:text (cider/handle-doc {:symbol "car" :session_name "el"}))))
      (is (= [[:bridge "el"]] @received)
          "its symbols live in Emacs, so no nREPL request is sent"))))

(deftest a-session-without-a-repl-buffer-falls-back-to-the-bridge
  (let [{:keys [received eval-fn]}
        (registry-stub [{:name "gone" :repl-type "clj" :cider-buffer nil}])]
    (binding [cider/*eval-fn* eval-fn]
      (cider/handle-doc {:symbol "map" :session_name "gone"})
      (is (= [[:bridge "gone"]] @received)))))

(deftest the-current-connection-refuses-cljel-and-falls-back
  (let [{:keys [received calls eval-fn]}
        (registry-stub [] (constantly "{\"refused\":\"cljel\"}"))]
    (binding [cider/*eval-fn* eval-fn]
      (is (= "{\"bridge\":true}" (:text (cider/handle-info {:symbol "car"}))))
      (is (= [[:nrepl :current] [:bridge :default]] @received))
      (is (str/includes? (first (filter #(str/includes? % "cider-nrepl-send-request") @calls))
                         "(\"cljel\")")
          "the boundary is told which REPL types to refuse"))))

(deftest an-unanswered-request-is-a-bounded-error
  (let [{:keys [eval-fn]} (registry-stub ["s1"] (constantly "{\"timeout\":10}"))]
    (binding [cider/*eval-fn* eval-fn]
      (let [resp (cider/handle-complete {:prefix "ma" :session_name "s1"})]
        (is (:isError resp))
        (is (str/includes? (:text resp) "no reply within 10s"))))))

(deftest resolve-named-session-lists-known-sessions
  (is (= {:ok "a"} (select-keys (cider/resolve-named-session [{:name "a"}] "a") [:ok])))
  (let [r (cider/resolve-named-session [] "x")]
    (is (= :cider/unknown-session (:error r)))
    (is (str/includes? (:message r) "(none)"))))

(deftest eval-auto-spawns-when-no-session
  (let [sessions-json "[]"
        {:keys [calls eval-fn]}
        (make-stub (fn [code]
                     (cond
                       (str/includes? code "list-sessions") {:success true :result sessions-json}
                       (str/includes? code "spawn-session-from-plist") {:success true :result "\"{}\""}
                       :else {:success true :result "\"3\""})))]
    (binding [cider/*eval-fn* eval-fn]
      (cider/handle-eval {:code "(+ 1 2)" :project_dir "/proj"})
      (is (some #(str/includes? % "spawn-session-from-plist") @calls)
          "no connected session for the project -> auto-spawn")
      (is (some #(str/includes? % "auto-") @calls)
          "spawn uses the auto-<hash> name"))))

;;; =============================================================================
;;; Emacs waiting for input: spawn says so instead of hanging silently
;;; =============================================================================

(def ^:private prompt-paragraph
  "Emacs daemon \"server\" is WAITING FOR INPUT (2s, live): a minibuffer prompt \"Reuse dead REPL? (y or n)\", opened by an emacsclient eval.")

(deftest spawn-names-a-prompt-that-is-already-waiting
  (let [{:keys [eval-fn]} (make-stub (fn [_] {:success true
                                              :result "{\"name\":\"dev\",\"status\":\"starting\"}"}))]
    (binding [cider/*eval-fn* eval-fn
              cider/*attention-fn* (constantly prompt-paragraph)]
      (let [resp (cider/handle-spawn {:name "dev"})]
        (is (not (:isError resp)))
        (is (str/includes? (:text resp) "\"status\":\"starting\""))
        (is (str/includes? (:text resp) "\"attention\""))
        (is (str/includes? (:text resp) "Reuse dead REPL"))))))

(deftest spawn-reply-is-untouched-when-nothing-waits
  (let [raw "{\"name\":\"dev\",\"status\":\"starting\"}"
        {:keys [eval-fn]} (make-stub (fn [_] {:success true :result raw}))]
    (binding [cider/*eval-fn* eval-fn
              cider/*attention-fn* (constantly nil)]
      (is (= raw (:text (cider/handle-spawn {:name "dev"})))))))

(deftest a-timeout-that-already-names-the-prompt-is-not-repeated
  (binding [cider/*attention-fn* (constantly prompt-paragraph)]
    (let [err {:error :cider/elisp-failed
               :message (str "Emacsclient call timed out after 5000ms\n"
                             (str/replace prompt-paragraph "2s" "0s"))}
          out (cider/with-waiting-prompt err)]
      (is (= 1 (count (re-seq #"WAITING FOR INPUT" (:message out))))))
    (let [out (cider/with-waiting-prompt {:error :cider/elisp-failed :message "boom"})]
      (is (str/includes? (:message out) "WAITING FOR INPUT")))))

(deftest auto-spawn-stops-waiting-when-jack-in-is-stuck-at-a-prompt
  (let [{:keys [calls eval-fn]}
        (make-stub (fn [code]
                     (cond
                       (str/includes? code "list-sessions") {:success true :result "[]"}
                       (str/includes? code "spawn-session-from-plist") {:success true :result "\"{}\""}
                       :else {:success true :result "\"3\""})))
        started (System/currentTimeMillis)]
    (binding [cider/*eval-fn* eval-fn
              cider/*attention-fn* (constantly prompt-paragraph)]
      (let [resp (cider/handle-eval {:code "(+ 1 2)" :project_dir "/proj"})]
        (is (true? (:isError resp)))
        (is (str/includes? (:text resp) "Emacs is waiting for input"))
        (is (str/includes? (:text resp) "Reuse dead REPL"))
        (is (< (count (filter #(str/includes? % "list-sessions") @calls)) 4)
            "the readiness budget is not spent polling a session that cannot connect")
        (is (< (- (System/currentTimeMillis) started) 5000))))))

(deftest spawn-readiness-budget-covers-a-cold-jvm-boot
  (let [poll     @#'cider/session-ready-poll-ms
        attempts @#'cider/session-ready-max-attempts
        budget-s (/ (* poll attempts) 1000)]
    (is (>= budget-s 30)
        (str "auto-spawn on eval boots a JVM nREPL and then waits this long for it to "
             "report connected; a cold JVM takes 20-40s, so a budget under 30s reports "
             ":cider/session-timeout for sessions that are merely still starting. "
             "Budget is " budget-s "s."))))

(deftest eval-reuses-connected-session
  (let [sessions-json "[{\"name\": \"auto-1\", \"status\": \"connected\", \"project-dir\": \"/proj\"}]"
        {:keys [calls eval-fn]}
        (make-stub (fn [code]
                     (if (str/includes? code "list-sessions")
                       {:success true :result sessions-json}
                       {:success true :result "\"3\""})))]
    (binding [cider/*eval-fn* eval-fn]
      (cider/handle-eval {:code "(+ 1 2)" :project_dir "/proj"})
      (is (not (some #(str/includes? % "spawn-session-from-plist") @calls))
          "a connected session for the project is reused, never respawned")
      (is (some #(str/includes? % "eval-in-session") @calls)))))

;;; =============================================================================
;;; ensure-connected — the ICiderPort auto-connect verb
;;; =============================================================================

(deftest ensure-connected-returns-connected-session-name
  (let [sessions-json "[{\"name\": \"auto-1\", \"status\": \"connected\", \"project-dir\": \"/proj\"}]"
        {:keys [eval-fn]}
        (make-stub (fn [_] {:success true :result sessions-json}))]
    (binding [cider/*eval-fn* eval-fn]
      (is (= "auto-1" (cider/ensure-connected "/proj"))))))

(deftest ensure-connected-throws-on-spawn-failure
  (let [{:keys [eval-fn]}
        (make-stub (fn [code]
                     (if (str/includes? code "list-sessions")
                       {:success true :result "[]"}
                       {:success false :error "daemon down"})))]
    (binding [cider/*eval-fn* eval-fn]
      (is (thrown? clojure.lang.ExceptionInfo
                   (cider/ensure-connected "/proj"))))))

;;; =============================================================================
;;; introspection — session arg normalization
;;; =============================================================================

(deftest doc-omits-blank-session
  (let [{:keys [received eval-fn]} (registry-stub ["s1"])]
    (binding [cider/*eval-fn* eval-fn]
      (cider/handle-doc {:symbol "map" :session_name "  "})
      (is (= [[:nrepl :current]] @received)
          "a blank name targets the current connection, without a registry lookup"))))

;;; =============================================================================
;;; connect — boundary validation
;;; =============================================================================

(deftest connect-requires-port
  (binding [cider/*eval-fn* (:eval-fn (ok-stub))]
    (let [resp (cider/handle-connect {:name "x"})]
      (is (true? (:isError resp))))))

;;; =============================================================================
;;; contribution shape
;;; =============================================================================

(deftest contribution-covers-core-verbs
  (let [verbs (set (keys cider/handlers))]
    (doseq [v [:eval :doc :info :complete :apropos :status :spawn :connect
               :sessions :kill-session :kill-all]]
      (is (contains? verbs v) (str "missing verb " v))))
  (is (ifn? (get-in cider/commands ["cider" :handler])))
  (is (map? cider/schema-params))
  (is (= #{"code" "mode" "timeout" "symbol" "prefix" "pattern" "search_docs"
           "session_name" "name" "host" "port" "project_dir" "agent_id"
           "repl_type" "extra_args" "aliases" "extra_deps" "middleware"}
         (set (keys cider/schema-params)))))

;;; =============================================================================
;;; contribute!/retract! — the injected :extension/* runtime ports do the work
;;; =============================================================================

(deftest contribute-uses-injected-runtime-port
  (let [calls (atom [])]
    (cider/contribute!
     {:extension/contribute-commands!
      (fn [tool addon cmds] (swap! calls conj [tool addon (keys cmds)]))})
    (is (= [["code" "hive.emacs" '("cider")]] @calls))))

(deftest contribute-noops-without-runtime-ports
  (is (false? (cider/contribute! nil))
      "no port injected: nothing reaches the host, and the caller is told so")
  (is (false? (cider/contribute! {:extension/register! (fn [_ _] nil)})))
  (is (nil? (cider/retract! nil))))

(deftest contribute-reports-delivery-through-the-port
  (let [seen (atom [])]
    (is (true? (cider/contribute! {:extension/contribute-commands!
                                   (fn [tool addon-id commands]
                                     (swap! seen conj [tool addon-id (set (keys commands))]))})))
    (is (= [["code" "hive.emacs" #{"cider"}]] @seen))))

(deftest retract-uses-injected-runtime-port
  (let [calls (atom [])]
    (cider/retract!
     {:extension/retract-contributions!
      (fn [addon] (swap! calls conj addon))})
    (is (= ["hive.emacs"] @calls))))
