(ns hive-emacs.tools.cider-test
  "Unit tests for hive-emacs.tools.cider — the addon-owned :cider verb tree.

   DIP-in-tests: the ONLY host effect is the elisp-eval boundary, injected via
   cider/*eval-fn*. The stub records every elisp form and answers canned
   responses per call — no Emacs, no hive-mcp, no nREPL."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-emacs.tools.cider :as cider]))
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
    (binding [cider/*eval-fn* eval-fn]
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

;;; =============================================================================
;;; kill-session — fail loud on blank (no silent no-op)
;;; =============================================================================

(deftest kill-session-rejects-blank
  (let [{:keys [calls eval-fn]} (ok-stub)]
    (binding [cider/*eval-fn* eval-fn]
      (let [resp (cider/handle-kill-session {:session_name nil})]
        (is (true? (:isError resp))))
      (is (empty? @calls)))))

;;; =============================================================================
;;; eval — session routing + auto-connect spawn
;;; =============================================================================

(deftest eval-routes-to-named-session
  (let [{:keys [calls eval-fn]} (ok-stub)]
    (binding [cider/*eval-fn* eval-fn]
      (cider/handle-eval {:code "(+ 1 2)" :session_name "s1"})
      (is (str/includes? (first @calls) "hive-mcp-cider-eval-in-session"))
      (is (str/includes? (first @calls) "\"s1\"")))))

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
;;; introspection — session arg normalization
;;; =============================================================================

(deftest doc-omits-blank-session
  (let [{:keys [calls eval-fn]} (ok-stub)]
    (binding [cider/*eval-fn* eval-fn]
      (cider/handle-doc {:symbol "map" :session_name "  "})
      (is (str/includes? (first @calls) "hive-mcp-cider-doc"))
      (is (str/includes? (first @calls) "\"map\" nil")))))

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
  (is (map? cider/spawn-schema-params))
  (is (= #{"extra_args" "aliases" "extra_deps" "middleware"}
         (set (keys cider/spawn-schema-params)))))
