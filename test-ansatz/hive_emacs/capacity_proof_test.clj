(ns hive-emacs.capacity-proof-test
  "Kernel proof for the saturating-capacity primitive used by ranking."
  (:require [ansatz.core :as a]
            [ansatz.kernel.env :as env]
            [ansatz.kernel.name :as kernel-name]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-emacs.daemon-scoring :as scoring]
            [malli.core :as m]))

;; Malli is the shared signature: Ansatz elaborates it as Nat -> Nat -> Nat.
;; The ordinary definition also keeps static code indexes honest before the
;; fixture installs the kernel-backed definition into the Ansatz environment.
(defn verifiedCapacityLeft
  [capacity used]
  (scoring/capacity-left capacity used))
(m/=> verifiedCapacityLeft [:=> [:cat nat-int? nat-int?] nat-int?])

(defonce ^:private booted
  (delay (binding [a/*verbose* false] (a/load-init!))))

(defn- ensure-proof-subject!
  []
  @booted
  (binding [a/*verbose* false
            *ns* (find-ns 'hive-emacs.capacity-proof-test)]
    (when-not (env/lookup (a/env)
                          (kernel-name/from-string "verifiedCapacityLeft"))
      (eval '(ansatz.core/defn verifiedCapacityLeft [capacity used]
               (- capacity used)))))
  nil)

(use-fixtures :once (fn [test-fn]
                      (ensure-proof-subject!)
                      (test-fn)))

(deftest verified-model-matches-production
  (testing "generated kernel subject and production primitive agree"
    (doseq [capacity (range 0 32)
            used (range 0 48)]
      (is (= (scoring/capacity-left capacity used)
             (long (verifiedCapacityLeft capacity used)))))))

(deftest subtract-zero-is-identity-for-all-capacities
  (testing "CIC kernel proves the production model law over every Nat"
    (a/theorem capacityLeftZero
      [capacity :- Nat]
      (= Nat ((verifiedCapacityLeft capacity) 0) capacity)
      (simp [verifiedCapacityLeft]))
    (is (some? (env/lookup (a/env)
                           (kernel-name/from-string "capacityLeftZero"))))))

(deftest remaining-capacity-never-exceeds-capacity
  (testing "CIC kernel proves the saturation bound over every Nat pair"
    (a/theorem capacityLeftBounded
      [capacity :- Nat, used :- Nat]
      (<= Nat ((verifiedCapacityLeft capacity) used) capacity)
      (simp [verifiedCapacityLeft])
      (omega))
    (is (some? (env/lookup (a/env)
                           (kernel-name/from-string "capacityLeftBounded"))))))
