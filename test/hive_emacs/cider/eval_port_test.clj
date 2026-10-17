(ns hive-emacs.cider.eval-port-test
  "An eval that names an nREPL (port param, else <directory>/.nrepl-port) runs
   in that nREPL and never spawns beside it; every port error names the port,
   where it came from and the resolution order.

   DIP-in-tests: the elisp boundary (cider/*eval-fn*), the attention boundary
   and the port-file boundary (eval-port/*port-file-fn*) are bound to stubs.
   No Emacs, no nREPL, no filesystem."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-emacs.cider.eval-port :as eval-port]
            [hive-emacs.tools.cider :as cider]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defn- emacs
  "A stub elisp boundary over a mutable session registry. connect! decides
   what a connect-session call does to the registry and whether it succeeds."
  [sessions connect!]
  (let [calls (atom [])
        reg   (atom sessions)
        answer (fn [code]
                 (swap! calls conj code)
                 (cond
                   (str/includes? code "hive-mcp-cider-list-sessions")
                   {:success true :result (json/write-str @reg)}

                   (str/includes? code "hive-mcp-cider-connect-session")
                   (connect! reg)

                   (str/includes? code "hive-mcp-cider-spawn-session-from-plist")
                   {:success false :error "spawn refused by stub"}

                   (str/includes? code "hive-mcp-cider-eval-in-session")
                   {:success true :result "=> 3"}

                   :else {:success true :result "nil"}))]
    {:calls calls
     :eval-fn (fn ([code] (answer code)) ([code _] (answer code)))}))

(defn- run [stub port-files params]
  (binding [cider/*eval-fn*          (:eval-fn stub)
            cider/*attention-fn*     (constantly nil)
            eval-port/*port-file-fn* (fn [dir] (get port-files dir))]
    (let [resp (cider/handle-eval params)]
      {:error? (boolean (:isError resp))
       :text   (str (:text resp) (get-in resp [:content 0 :text]))})))

(defn- sent [stub fragment]
  (filter #(str/includes? % fragment) @(:calls stub)))

(def ^:private refuse-connect (fn [_] {:success false :error "Connection refused"}))

(deftest the-port-param-picks-the-session-on-that-port
  (let [stub (emacs [{:name "other" :port 7921 :status "connected" :project-dir "/w"}
                     {:name "mine" :port 7944 :status "connected" :project-dir "/elsewhere"}]
                    refuse-connect)
        out  (run stub {} {:code "(+ 1 2)" :directory "/w" :port 7944})]
    (is (not (:error? out)) (:text out))
    (is (= 1 (count (sent stub "hive-mcp-cider-eval-in-session"))))
    (is (str/includes? (first (sent stub "hive-mcp-cider-eval-in-session")) "\"mine\"")
        "the port wins over the session the directory would route to")
    (is (empty? (sent stub "spawn-session")))
    (is (empty? (sent stub "connect-session")))))

(deftest the-worktree-port-file-is-connected-to-never-spawned-beside
  (let [stub (emacs [] (fn [reg]
                         (swap! reg conj {:name "port-7946" :port 7946 :status "connected"})
                         {:success true :result "{}"}))
        out  (run stub {"/wt" "7946\n"} {:code "(+ 1 2)" :directory "/wt"})]
    (is (not (:error? out)) (:text out))
    (is (str/includes? (first (sent stub "connect-session")) "7946"))
    (is (str/includes? (first (sent stub "hive-mcp-cider-eval-in-session")) "\"port-7946\""))
    (is (empty? (sent stub "spawn-session")))))

(deftest an-unreachable-port-names-port-source-and-order
  (testing "from the port file"
    (let [stub (emacs [] refuse-connect)
          out  (run stub {"/wt" "7946"} {:code "1" :directory "/wt"})]
      (is (:error? out))
      (doseq [s ["7946" "/wt/.nrepl-port" "Nothing was spawned" "Resolution order" "Connection refused"]]
        (is (str/includes? (:text out) s) s))
      (is (empty? (sent stub "spawn-session")))
      (is (empty? (sent stub "eval-in-session")))))
  (testing "from the caller, which says where it got the port"
    (let [stub (emacs [] refuse-connect)
          out  (run stub {} {:code "1" :directory "/wt" :port 7950 :port_source "CLOJURE_EVAL_PORT"})]
      (is (str/includes? (:text out) "7950"))
      (is (str/includes? (:text out) "CLOJURE_EVAL_PORT")))))

(deftest a-port-that-is-not-a-port-is-refused-before-any-elisp
  (doseq [[files params] [[{"/wt" "garbage"} {:code "1" :directory "/wt"}]
                          [{} {:code "1" :directory "/wt" :port 70000}]]]
    (let [stub (emacs [] refuse-connect)
          out  (run stub files params)]
      (is (:error? out))
      (is (str/includes? (:text out) "not a TCP port"))
      (is (str/includes? (:text out) "Resolution order"))
      (is (empty? @(:calls stub))))))

(deftest no-port-anywhere-spawns-heap-capped-and-says-why
  (let [stub (emacs [] refuse-connect)
        out  (run stub {} {:code "1" :directory "/wt"})]
    (is (:error? out))
    (is (str/includes? (first (sent stub "spawn-session")) "-J-Xmx2g"))
    (is (str/includes? (:text out) "no port param and no /wt/.nrepl-port"))
    (is (str/includes? (:text out) "Resolution order"))))

(deftest resolve-port-order
  (binding [eval-port/*port-file-fn* {"/wt" "7946"}]
    (is (= {:port 7000 :source "the port param"} (:ok (eval-port/resolve-port {:port "7000"} "/wt"))))
    (is (= {:port 7946 :source "/wt/.nrepl-port"} (:ok (eval-port/resolve-port {} "/wt"))))
    (is (nil? (:ok (eval-port/resolve-port {} "/none"))))
    (is (nil? (:ok (eval-port/resolve-port {} nil))))))
