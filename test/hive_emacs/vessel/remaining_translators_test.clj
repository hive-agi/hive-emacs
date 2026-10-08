(ns hive-emacs.vessel.remaining-translators-test
  (:require [clojure.test :refer [is]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-emacs.vessel.dispatch :as dispatch]
            [hive-emacs.magit.translators :as magit]
            [hive-emacs.projectile.translators :as projectile]
            [hive-emacs.memory.translators :as memory]
            [hive-vessel.core :as core]))

(defn- plan [op]
  (core/plan (dispatch/registry) (:emacs core/reference-targets) op))
(defn- payload [op]
  (get-in (plan op) [:ok :plan/ops 0 :native/payload]))

(deftrifecta closed-remaining-operations
  plan
  {:gen (gen/elements [{:op :magit/status :directory "/tmp/repo"}
                       {:op :project/files :pattern nil}
                       {:op :memory/legacy-export :project-id "hive"}])
   :pred #(contains? % :ok)
   :num-tests 30
   :mutations [["no-translation" (fn [_] {:error :unavailable})]]
   :assert (fn []
             (is (= "(json-encode (list :notes (hive-mcp-memory-query 'note nil \"hive\" 1000 nil t) :snippets (hive-mcp-memory-query 'snippet nil \"hive\" 1000 nil t) :conventions (hive-mcp-memory-query 'convention nil \"hive\" 1000 nil t) :decisions (hive-mcp-memory-query 'decision nil \"hive\" 1000 nil t)))"
                    (payload {:op :memory/legacy-export :project-id "hive"})))
             (is (every? #(contains? (set (map :translator/op dispatch/translators)) %)
                         (concat (map :translator/op magit/translators)
                                 (map :translator/op projectile/translators)
                                 (map :translator/op memory/translators))))
             (is (contains? (plan {:op :magit/status :directory ""}) :error))
             (is (contains? (plan {:op :memory/legacy-export :project-id ""}) :error))
             (is (contains? (plan {:op :project/search :pattern 1}) :error))
             (is (contains? (plan {:op :magit/eval :code "(kill-emacs)"}) :error))
             (is (.contains (payload {:op :magit/push :directory "/tmp/repo" :remote "x\") (kill-emacs)" :set-upstream true}) "\\\""))
             (is (.contains (payload {:op :magit/stage-verify :directory "/tmp/repo" :paths ["a"]}) "shell-quote-argument")))})
