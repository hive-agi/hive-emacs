(ns hive-emacs.schema-proof-test
  "Malli-derived generative, relational, contract, and mutation checks."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-emacs.daemon-scoring :as scoring]
            [hive-emacs.schema]
            [hive-schemas.test :as schema-test]
            [malli.core :as m]))

(defn- coherent-score?
  [_input {:keys [health-score health-level]}]
  (and (<= 0 health-score 100)
       (= health-level (scoring/health-level health-score))))

(schema-test/deftrifecta-from-schema score-snapshot-proof
  scoring/score-snapshot
  {:in :hive-emacs/score-input
   :out :hive-emacs/score-snapshot
   :rel coherent-score?
   :contract true
   :mutation true
   :num-tests 100
   :seed 42
   :n-cases 10})

(deftest function-schemas-are-registered
  (testing "proof subjects have executable Malli contracts"
    (is (get-in (m/function-schemas)
                ['hive-emacs.daemon-scoring 'score-snapshot :schema]))
    (is (get-in (m/function-schemas)
                ['hive-emacs.daemon-scoring 'capacity-left :schema]))))

(deftest capacity-subtraction-laws
  (doseq [capacity (range 0 25)
          used (range 0 35)]
    (let [remaining (scoring/capacity-left capacity used)]
      (is (nat-int? remaining))
      (is (<= remaining capacity))
      (is (= remaining (max 0 (- capacity used)))))))
