(ns hive-emacs.project.translators-test
  "Project ops lower to exact :elisp natives through the hive-vessel registry,
   and malformed ops are rejected by :translator/accepts."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-emacs.project.translators :as project]
            [hive-vessel.core :as vcore]
            [hive-vessel.dialect.elisp :as el]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def ^:private emacs (:emacs vcore/reference-targets))

(def ^:private registry (vcore/standard-registry project/translators))

(defn- payloads
  "The native :elisp payloads OP plans to for the Emacs target."
  [op]
  (let [{:keys [ok error]} (vcore/plan registry emacs op)]
    (when error (throw (ex-info "plan failed" error)))
    (mapv (fn [{:native/keys [dialect payload]}]
            (is (= :elisp dialect))
            payload)
          (:plan/ops ok))))

(deftest project-info-lowers-to-the-exact-native-payload
  (testing "without :directory"
    (is (= ["(progn (require 'hive-mcp-projectile) (json-encode (hive-mcp-projectile-api-project-info)))"]
           (payloads {:op :project/info}))))
  (testing "with :directory, default-directory is let-bound with a trailing slash"
    (is (= ["(let ((default-directory \"/tmp/proj/\")) (progn (require 'hive-mcp-projectile) (json-encode (hive-mcp-projectile-api-project-info))))"]
           (payloads {:op :project/info :directory "/tmp/proj"})))))

(deftest interpolations-are-escaped-by-the-dialect
  (let [nasty "a\"b\\c"
        dir (str nasty "/x")]
    (is (= [(str "(let ((default-directory " (el/string-literal (str dir "/")) ")) "
                "(progn (require 'hive-mcp-projectile) (json-encode (hive-mcp-projectile-api-project-info))))")]
           (payloads {:op :project/info :directory dir})))))

(deftest every-translator-is-covered
  (is (= (set (map :translator/op project/translators))
         #{:project/info})))

(deftest malformed-ops-do-not-plan
  (doseq [op [{:op :project/info :directory 7}
              {:op :project/info :directory ""}
              {:op :project/info :directory "   "}
              {:op :project/no-such-op}]]
    (is (contains? (vcore/plan registry emacs op) :error) (pr-str op))))
