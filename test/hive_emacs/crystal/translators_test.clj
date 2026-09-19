(ns hive-emacs.crystal.translators-test
  "Crystal ops lower to exact :elisp natives through the hive-vessel registry,
   and malformed ops are rejected by :translator/accepts."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-emacs.crystal.translators :as crystal]
            [hive-vessel.core :as vcore]
            [hive-vessel.dialect.elisp :as el]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def ^:private emacs (:emacs vcore/reference-targets))

(def ^:private registry (vcore/standard-registry crystal/translators))

(defn- payloads
  "The native :elisp payloads OP plans to for the Emacs target."
  [op]
  (let [{:keys [ok error]} (vcore/plan registry emacs op)]
    (when error (throw (ex-info "plan failed" error)))
    (mapv (fn [{:native/keys [dialect payload]}]
            (is (= :elisp dialect))
            payload)
          (:plan/ops ok))))

(deftest crystal-available-lowers-to-featurep
  (is (= ["(featurep 'hive-mcp)"]
         (payloads {:op :crystal/available?}))))

(deftest crystal-wrap-gather-lowers-to-the-exact-native-payload
  (testing "without :directory"
    (is (= ["(json-encode (hive-mcp-api-wrap-gather))"]
           (payloads {:op :crystal/wrap-gather}))))
  (testing "with :directory"
    (is (= ["(json-encode (hive-mcp-api-wrap-gather \"/tmp/proj\"))"]
           (payloads {:op :crystal/wrap-gather :directory "/tmp/proj"})))))

(deftest crystal-session-notes-lowers-to-the-exact-native-payload
  (testing "without :project-id"
    (is (= ["(json-encode (hive-mcp-memory-query 'note nil nil 50 'ephemeral nil))"]
           (payloads {:op :crystal/session-notes}))))
  (testing "with :project-id"
    (is (= ["(json-encode (hive-mcp-memory-query 'note nil \"proj-1\" 50 'ephemeral nil))"]
           (payloads {:op :crystal/session-notes :project-id "proj-1"})))))

(deftest crystal-kanban-notes-lowers-to-the-exact-native-payload
  (testing "without :project-id"
    (is (= [(str "(json-encode (append "
                 "(hive-mcp-memory-query 'note '(\"kanban\") nil 50 'ephemeral nil) "
                 "(hive-mcp-memory-query 'note '(\"kanban\") nil 50 'short-term nil)))")]
           (payloads {:op :crystal/kanban-notes}))))
  (testing "with :project-id"
    (is (= [(str "(json-encode (append "
                 "(hive-mcp-memory-query 'note '(\"kanban\") \"proj-1\" 50 'ephemeral nil) "
                 "(hive-mcp-memory-query 'note '(\"kanban\") \"proj-1\" 50 'short-term nil)))")]
           (payloads {:op :crystal/kanban-notes :project-id "proj-1"})))))

(deftest crystal-git-commits-lowers-to-the-exact-native-payload
  (testing "without :directory or :since, since defaults to midnight"
    (is (= [(str "(shell-command-to-string "
                 "\"git log --since='midnight' --oneline 2>/dev/null\")")]
           (payloads {:op :crystal/git-commits}))))
  (testing "with an ISO-instant :since"
    (is (= [(str "(shell-command-to-string "
                 "\"git log --since='2026-09-19T12:00:00Z' --oneline 2>/dev/null\")")]
           (payloads {:op :crystal/git-commits :since "2026-09-19T12:00:00Z"}))))
  (testing "with :directory and :since, default-directory is let-bound"
    (is (= [(str "(let ((default-directory \"/tmp/proj\")) "
                 "(shell-command-to-string \"git log --since='2026-09-19T12:00:00Z' --oneline 2>/dev/null\"))")]
           (payloads {:op :crystal/git-commits
                      :directory "/tmp/proj"
                      :since "2026-09-19T12:00:00Z"})))))

(deftest interpolations-are-escaped-by-the-dialect
  (let [nasty "a\"b\\c"]
    (is (= [(str "(json-encode (hive-mcp-api-wrap-gather "
                 (el/string-literal nasty) "))")]
           (payloads {:op :crystal/wrap-gather :directory nasty})))
    (is (= [(str "(let ((default-directory " (el/string-literal nasty) ")) "
                 "(shell-command-to-string \"git log --since='midnight' --oneline 2>/dev/null\"))")]
           (payloads {:op :crystal/git-commits :directory nasty})))))

(deftest every-translator-is-covered
  (is (= (set (map :translator/op crystal/translators))
         #{:crystal/available?
           :crystal/wrap-gather
           :crystal/session-notes
           :crystal/kanban-notes
           :crystal/git-commits})))

(deftest malformed-ops-do-not-plan
  (doseq [op [{:op :crystal/wrap-gather :directory 7}
              {:op :crystal/wrap-gather :directory ""}
              {:op :crystal/wrap-gather :directory "   "}
              {:op :crystal/session-notes :project-id 7}
              {:op :crystal/session-notes :project-id ""}
              {:op :crystal/session-notes :project-id "   "}
              {:op :crystal/kanban-notes :project-id 7}
              {:op :crystal/kanban-notes :project-id ""}
              {:op :crystal/kanban-notes :project-id "   "}
              {:op :crystal/git-commits :directory 7}
              {:op :crystal/git-commits :directory ""}
              {:op :crystal/git-commits :directory "   "}
              {:op :crystal/no-such-op}]]
    (is (contains? (vcore/plan registry emacs op) :error) (pr-str op))))
