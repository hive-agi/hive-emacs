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

(deftest the-emacs-tool-reaches-multi-dispatch
  ;; The addon claims `emacs` over the host's core tool. tools/list follows the
  ;; claim on its own; `multi tool=emacs` only follows a :multi/tool entry, so
  ;; the commands the host seed never knew (attention, answer) need this hook.
  (let [entries (:multi/tool mh/contributions)
        entry (first entries)]
    (is (= ["emacs"] (mapv :tool-name entries)))
    (is (ifn? (:handler entry)))
    (let [help (pr-str ((:handler entry) {:command "help"}))]
      (doseq [command ["attention" "answer" "eval"]]
        (is (.contains ^String help command) (str command " is dispatchable through multi"))))))

(deftest no-duplicate-codes
  (let [codes (map :code mh/emacs-verbs)]
    (is (= (count codes) (count (distinct codes))))))

(deftest em-prefix-discipline
  (testing "every emacs verb starts with the em- prefix"
    (doseq [v mh/emacs-verbs]
      (is (.startsWith ^String (:code v) "em")))))
