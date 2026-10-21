(ns hive-emacs.lsp-bridge-test
  "The Emacs live LSP strategy over a stub evaluator: no Emacs, no lsp-mcp."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-emacs.bridge-loader :as loader]
            [hive-emacs.lsp-bridge :as lsp-bridge]))

;; SPDX-License-Identifier: MIT

(defn- stub-eval
  "Evaluator double: every bridge-load form succeeds; the LSP call answers
   `answer`. Every code string lands in `seen`."
  [seen answer]
  (fn [code _timeout]
    (swap! seen conj code)
    (if (str/includes? code "hive-lsp-bridge-")
      answer
      {:success true :result "t"})))

(deftest a-call-names-the-elisp-function-and-parses-its-json
  (loader/invalidate-ready!)
  (let [seen (atom [])
        res  (lsp-bridge/lsp-call (stub-eval seen {:success true :result "{\"contents\":\"doc\"}"})
                                  "hover" "/p" "/p/a.clj" 3 7)]
    (is (= {:contents "doc"} res))
    (let [call (last @seen)]
      (is (str/includes? call "(require 'hive-lsp-bridge nil t)"))
      (is (str/includes? call "(hive-lsp-bridge-hover \"/p\" \"/p/a.clj\" 3 7)")))))

(deftest failures-come-back-as-error-maps
  (loader/invalidate-ready!)
  (testing "Emacs refused"
    (is (= {:error "boom"}
           (lsp-bridge/lsp-call (stub-eval (atom []) {:success false :error "boom"}) "status"))))
  (testing "not JSON"
    (is (str/starts-with? (:error (lsp-bridge/lsp-call (stub-eval (atom []) {:success true :result "nope"})
                                                       "status"))
                          "JSON parse error"))))

(deftest registering-without-lsp-mcp-is-a-quiet-no
  (if (try (requiring-resolve 'lsp-mcp.bridge/register-bridge!) (catch Exception _ nil))
    (is true "lsp-mcp is on this classpath; the absent case cannot be observed here")
    (do (is (false? (lsp-bridge/register! (stub-eval (atom []) {:success true :result "{}"}))))
        (is (nil? (lsp-bridge/unregister!))))))
