(ns hive-emacs.kanban.translators-test
  "Kanban ops lower to exact :elisp natives through the hive-vessel registry,
   and malformed ops are rejected by :translator/accepts."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-emacs.kanban.translators :as kanban]
            [hive-vessel.core :as vcore]
            [hive-vessel.dialect.elisp :as el]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def ^:private emacs (:emacs vcore/reference-targets))

(def ^:private registry (vcore/standard-registry kanban/translators))

(defn- payloads
  "The native :elisp payloads OP plans to for the Emacs target."
  [op]
  (let [{:keys [ok error]} (vcore/plan registry emacs op)]
    (when error (throw (ex-info "plan failed" error)))
    (mapv (fn [{:native/keys [dialect payload]}]
            (is (= :elisp dialect))
            payload)
          (:plan/ops ok))))

(deftest kanban-move-to-done-lowers-to-the-exact-native-payload
  (testing "without :directory"
    (is (= ["(json-encode (hive-mcp-api-kanban-move \"task-1\" \"done\" nil))"]
           (payloads {:op :kanban/move-to-done :task-id "task-1"}))))
  (testing "with :directory, it is the api's third argument"
    (is (= ["(json-encode (hive-mcp-api-kanban-move \"task-1\" \"done\" \"/tmp/proj\"))"]
           (payloads {:op :kanban/move-to-done :task-id "task-1" :directory "/tmp/proj"})))))

(deftest interpolations-are-escaped-by-the-dialect
  (let [nasty "a\"b\\c"]
    (is (= [(str "(json-encode (hive-mcp-api-kanban-move "
                 (el/string-literal nasty) " \"done\" nil))")]
           (payloads {:op :kanban/move-to-done :task-id nasty})))))

(deftest every-translator-is-covered
  (is (= (set (map :translator/op kanban/translators))
         #{:kanban/move-to-done})))

(deftest malformed-ops-do-not-plan
  (doseq [op [{:op :kanban/move-to-done :task-id 7}
              {:op :kanban/move-to-done :task-id ""}
              {:op :kanban/move-to-done :task-id "   "}
              {:op :kanban/move-to-done}
              {:op :kanban/no-such-op :task-id "task-1"}]]
    (is (contains? (vcore/plan registry emacs op) :error) (pr-str op))))
