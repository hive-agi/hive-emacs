(ns hive-emacs.cider.heap-cap-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-emacs.cider.alias-compose :as alias-compose]
            [hive-emacs.cider.heap-cap :as heap-cap]
            [hive-emacs.cider.spawn-dir :as spawn-dir]
            [hive-emacs.tools.cider :as cider]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(deftest with-heap-cap-names-a-ceiling
  (testing "no cap anywhere: the default is appended"
    (is (= [heap-cap/default-heap] (heap-cap/with-heap-cap nil nil false)))
    (is (= ["-Srepro" heap-cap/default-heap] (heap-cap/with-heap-cap "clj" ["-Srepro"] false))))
  (testing "the caller's -J-Xmx wins and is not doubled"
    (is (= ["-J-Xmx1500m"] (heap-cap/with-heap-cap "clj" ["-J-Xmx1500m"] false))))
  (testing "a bare -Xmx is given the -J the clojure CLI needs"
    (is (= ["-J-Xmx1500m"] (heap-cap/with-heap-cap "clj" ["-Xmx1500m"] false)))
    (is (= ["-J-Xms256m" heap-cap/default-heap] (heap-cap/with-heap-cap "clj" ["-Xms256m"] false))))
  (testing "an alias that caps the heap keeps its own cap"
    (is (nil? (heap-cap/with-heap-cap "clj" nil true))))
  (testing "non-JVM launches are untouched"
    (is (nil? (heap-cap/with-heap-cap "cljs" nil false)))
    (is (= ["x"] (heap-cap/with-heap-cap "cljw" ["x"] false)))))

(deftest alias-capped-reads-selected-aliases-only
  (let [deps {:aliases {:test {:jvm-opts ["-Xmx3g"]} :dev {:extra-paths ["dev"]}}}]
    (is (heap-cap/alias-capped? deps ["dev" "test"]))
    (is (not (heap-cap/alias-capped? deps ["dev"])))
    (is (not (heap-cap/alias-capped? nil ["test"])))))

(deftest spawn-launches-with-a-heap-cap
  (let [calls (atom [])
        eval-fn (fn ([code] (swap! calls conj code) {:success true :result "\"{}\""})
                  ([code _] (swap! calls conj code) {:success true :result "\"{}\""}))]
    (binding [cider/*eval-fn* eval-fn
              spawn-dir/*directory?* #{"/p" "/capped"}
              alias-compose/*read-deps-fn* {"/p" "{:aliases {:test {:extra-paths [\"test\"]}}}"
                                            "/capped" "{:aliases {:test {:jvm-opts [\"-Xmx3g\"]}}}"}]
      (cider/handle-spawn {:name "a" :project_dir "/p" :aliases ["test"]})
      (cider/handle-spawn {:name "b" :project_dir "/p" :aliases ["test"] :extra_args ["-J-Xmx1500m"]})
      (cider/handle-spawn {:name "c" :project_dir "/capped" :aliases ["test"]})
      (let [[a b c] @calls]
        (testing "the default ceiling is paired with a soft and initial heap no larger than it"
          (is (str/includes? a ":extra-args '(\"-J-Xmx2g\" \"-J-XX:SoftMaxHeapSize=2g\" \"-J-Xms512m\")") a))
        (testing "a caller's lower ceiling replaces the default and lowers the soft heap with it"
          (is (str/includes? b ":extra-args '(\"-J-Xmx1500m\" \"-J-XX:SoftMaxHeapSize=1500m\" \"-J-Xms512m\")") b))
        (is (not (str/includes? c ":extra-args")) c)))))
