(ns hive-emacs.editor.translators-test
  "Editor probe ops lower to exact :elisp natives through the hive-vessel
   registry, and anything outside the feature-symbol charset is rejected by
   :translator/accepts before any elisp is built."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-emacs.editor.translators :as editor]
            [hive-vessel.core :as vcore]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def ^:private emacs (:emacs vcore/reference-targets))

(def ^:private registry (vcore/standard-registry editor/translators))

(defn- payloads
  "The native :elisp payloads OP plans to for the Emacs target."
  [op]
  (let [{:keys [ok error]} (vcore/plan registry emacs op)]
    (when error (throw (ex-info "plan failed" error)))
    (mapv (fn [{:native/keys [dialect payload]}]
            (is (= :elisp dialect))
            payload)
          (:plan/ops ok))))

(deftest feature-probe-lowers-to-featurep
  (testing "plain feature"
    (is (= ["(featurep 'hive-mcp)"]
           (payloads {:op :editor/feature? :feature "hive-mcp"}))))
  (testing "the full admitted charset goes through the dialect's symbol escaping"
    (is (= ["(featurep 'hive-mcp-org-kanban)"]
           (payloads {:op :editor/feature? :feature "hive-mcp-org-kanban"})))
    ;; \. \: \@ read back as . : @, so the symbol NAME is unchanged
    (is (= ["(featurep 'a1+*\\./_\\:\\@~-z)"]
           (payloads {:op :editor/feature? :feature "a1+*./_:@~-z"})))))

(deftest every-translator-is-covered
  (is (= (set (map :translator/op editor/translators))
         #{:editor/feature?})))

(deftest malformed-ops-do-not-plan
  (doseq [op [{:op :editor/feature?}
              {:op :editor/feature? :feature ""}
              {:op :editor/feature? :feature 7}
              {:op :editor/feature? :feature "-leading-dash"}
              {:op :editor/feature? :feature "x) (delete-file \"/\""}
              {:op :editor/feature? :feature "a b"}
              {:op :editor/feature? :feature "x\n(kill-emacs)"}
              {:op :editor/feature? :feature (apply str (repeat 257 "a"))}
              {:op :editor/eval :code "(kill-emacs)"}]]
    (is (contains? (vcore/plan registry emacs op) :error) (pr-str op))))
