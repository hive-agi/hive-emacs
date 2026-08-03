(ns hive-emacs.olympus-test
  "Layout contract for hive-emacs.olympus: cell allocation, empty-cell skipping
   and tab overflow. Ling maps are keyed by :slave/id."
  (:require [clojure.test :refer [deftest testing is]]
            [hive-emacs.olympus :as olympus]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(deftest calculate-layout-grid-sizes
  (testing "n=1 is full screen"
    (let [layout (olympus/calculate-layout 1)]
      (is (= 1 (:rows layout)))
      (is (= 1 (:cols layout)))
      (is (nil? (:empty-cells layout)))
      (is (nil? (:tabs layout)))))
  (testing "n=2 is side-by-side"
    (let [layout (olympus/calculate-layout 2)]
      (is (= 1 (:rows layout)))
      (is (= 2 (:cols layout)))
      (is (nil? (:empty-cells layout)))
      (is (nil? (:tabs layout)))))
  (testing "n=3 is 2x2 with the last cell blanked"
    (let [layout (olympus/calculate-layout 3)]
      (is (= 2 (:rows layout)))
      (is (= 2 (:cols layout)))
      (is (= #{[1 1]} (:empty-cells layout)))
      (is (nil? (:tabs layout)))))
  (testing "n=4 is a perfect 2x2"
    (let [layout (olympus/calculate-layout 4)]
      (is (= 2 (:rows layout)))
      (is (= 2 (:cols layout)))
      (is (nil? (:empty-cells layout)))
      (is (nil? (:tabs layout))))))

(deftest calculate-layout-tabs-past-four
  (testing "n=5 opens a second tab"
    (let [layout (olympus/calculate-layout 5)]
      (is (= 2 (:tabs layout)))
      (is (= 4 (:per-tab layout)))))
  (testing "n=8 still fits two tabs"
    (is (= 2 (:tabs (olympus/calculate-layout 8)))))
  (testing "n=9 needs three"
    (is (= 3 (:tabs (olympus/calculate-layout 9))))))

(deftest calculate-layout-edge-cases
  (testing "n=0 is an empty grid"
    (let [layout (olympus/calculate-layout 0)]
      (is (= 0 (:rows layout)))
      (is (= 0 (:cols layout)))))
  (testing "negative n degrades to empty rather than throwing"
    (is (= 0 (:rows (olympus/calculate-layout -1)))))
  (testing "nil n degrades to empty"
    (is (= 0 (:rows (olympus/calculate-layout nil))))))

(deftest assign-positions-fills-cells-in-order
  (testing "one ling takes the only cell"
    (let [positions (olympus/assign-positions
                     [{:slave/id "ling-1" :slave/name "worker"}]
                     {:rows 1 :cols 1})]
      (is (= 1 (count positions)))
      (is (= {:row 0 :col 0 :tab nil} (get positions "ling-1")))))
  (testing "two lings sit side by side"
    (let [positions (olympus/assign-positions
                     [{:slave/id "ling-1" :slave/name "left"}
                      {:slave/id "ling-2" :slave/name "right"}]
                     {:rows 1 :cols 2})]
      (is (= 2 (count positions)))
      (is (= {:row 0 :col 0 :tab nil} (get positions "ling-1")))
      (is (= {:row 0 :col 1 :tab nil} (get positions "ling-2"))))))

(deftest assign-positions-skips-empty-cells
  (let [positions (olympus/assign-positions
                   [{:slave/id "ling-1"} {:slave/id "ling-2"} {:slave/id "ling-3"}]
                   {:rows 2 :cols 2 :empty-cells #{[1 1]}})]
    (is (= 3 (count positions)))
    (is (= {:row 0 :col 0 :tab nil} (get positions "ling-1")))
    (is (= {:row 0 :col 1 :tab nil} (get positions "ling-2")))
    (is (= {:row 1 :col 0 :tab nil} (get positions "ling-3")))
    (is (not-any? #(= {:row 1 :col 1 :tab nil} %) (vals positions)))))

(deftest assign-positions-overflows-into-tabs
  (let [lings (mapv #(hash-map :slave/id (str "ling-" %)) (range 1 6))
        positions (olympus/assign-positions lings {:tabs 2 :per-tab 4})]
    (is (= 5 (count positions)))
    (is (= 0 (:tab (get positions "ling-1"))))
    (is (= 0 (:tab (get positions "ling-4"))))
    (is (= 1 (:tab (get positions "ling-5"))))))

(deftest assign-positions-empty-input
  (is (= {} (olympus/assign-positions [] {:rows 2 :cols 2}))))

(deftest grid-capacity-and-tabbed
  (testing "capacity subtracts blanked cells"
    (is (= 3 (olympus/grid-capacity {:rows 2 :cols 2 :empty-cells #{[1 1]}})))
    (is (= 4 (olympus/grid-capacity {:rows 2 :cols 2})))
    (is (= 0 (olympus/grid-capacity {:rows 0 :cols 0}))))
  (testing "capacity of a tabbed layout is tabs * per-tab"
    (is (= 8 (olympus/grid-capacity {:tabs 2 :per-tab 4}))))
  (testing "tabbed? keys off :tabs"
    (is (true? (olympus/tabbed? {:tabs 2 :per-tab 4})))
    (is (false? (olympus/tabbed? {:rows 2 :cols 2})))))

(deftest position-for-cell-reverse-lookup
  (let [lings (mapv #(hash-map :slave/id (str "ling-" %)) (range 1 5))
        layout (olympus/calculate-layout 4)
        positions (olympus/assign-positions lings layout)]
    (is (= "ling-1" (olympus/position-for-cell positions 0 0 nil)))
    (is (= "ling-4" (olympus/position-for-cell positions 1 1 nil)))
    (is (nil? (olympus/position-for-cell positions 5 5 nil)))
    (is (nil? (olympus/position-for-cell positions 0 0 0))
        "a tab mismatch must not match a non-tabbed position")))

(deftest full-layout-flow
  (let [lings (mapv #(hash-map :slave/id (str "ling-" %)
                               :slave/name (str "worker-" %))
                    (range 1 5))
        layout (olympus/calculate-layout 4)
        positions (olympus/assign-positions lings layout)]
    (is (= 2 (:rows layout)))
    (is (= 2 (:cols layout)))
    (is (= 4 (count positions)))
    (is (= #{{:row 0 :col 0 :tab nil} {:row 0 :col 1 :tab nil}
             {:row 1 :col 0 :tab nil} {:row 1 :col 1 :tab nil}}
           (set (vals positions))))))
