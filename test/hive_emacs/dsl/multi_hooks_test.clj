(ns hive-emacs.dsl.multi-hooks-test
  "Shape + invariant tests for host-neutral `:multi/*` contributions."
  (:require [clojure.test :refer [deftest is testing]]
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

(deftest em-prefix-discipline
  (testing "every emacs verb starts with the em- prefix"
    (doseq [v mh/emacs-verbs]
      (is (.startsWith ^String (:code v) "em")))))
