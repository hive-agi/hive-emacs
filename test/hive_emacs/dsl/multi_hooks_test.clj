(ns hive-emacs.dsl.multi-hooks-test
  "Shape + invariant tests for the `:multi/*` IAddon contributions owned by
   `hive.emacs`. Guards the contribution data shape; addon → registry
   integration is covered at the hive-mcp layer."
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is testing]]
            [hive-emacs.dsl.multi-hooks :as mh]))

(deftest contributions-shape
  (testing "top-level keys are :multi/* namespaced"
    (is (every? #(= "multi" (namespace %)) (keys mh/contributions))))
  (testing "verb entries"
    (doseq [v mh/emacs-verbs]
      (is (string? (:code v)))
      (is (<= 1 (count (:code v)) 3))
      (is (= "emacs" (:tool v)))
      (is (string? (:command v))))))

(deftest no-duplicate-codes
  (let [codes (map :code mh/emacs-verbs)]
    (is (= (count codes) (count (distinct codes))))))

(deftest no-collision-with-core-verbs
  (when-let [core-table (try (require 'hive-mcp.dsl.verbs)
                             (some-> (find-var 'hive-mcp.dsl.verbs/verb-table) deref)
                             (catch Throwable _ nil))]
    (let [core-codes (set (keys core-table))
          em-codes   (set (map :code mh/emacs-verbs))]
      (is (empty? (set/intersection core-codes em-codes))
          (str "collisions: " (set/intersection core-codes em-codes))))))

(deftest em-prefix-discipline
  (testing "every emacs verb starts with the em- prefix"
    (doseq [v mh/emacs-verbs]
      (is (.startsWith ^String (:code v) "em")))))
