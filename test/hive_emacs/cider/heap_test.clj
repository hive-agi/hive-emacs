(ns hive-emacs.cider.heap-test
  "A spawn that lowers -Xmx must not start a JVM whose inherited
   SoftMaxHeapSize or initial heap exceeds that ceiling."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-emacs.cider.heap :as heap]
            [hive-emacs.tools.cider :as cider]
            [hive-emacs.cider.spawn :as spawn]
            [hive-emacs.cider.spawn-dir :as spawn-dir]
            [clojure.string :as str]))

(deftest parse-size-units
  (is (= (* 2 1024 1024 1024) (heap/parse-size "2g")))
  (is (= (* 512 1024 1024) (heap/parse-size "512M")))
  (is (= (* 64 1024) (heap/parse-size "64k")))
  (is (= 1000 (heap/parse-size "1000")))
  (is (nil? (heap/parse-size "lots"))))

(deftest lower-xmx-also-lowers-soft-and-initial-heap
  (is (= {:ok ["-J-Xmx2g" "-J-XX:SoftMaxHeapSize=2g" "-J-Xms512m"]}
         (heap/pair-heap-args ["-J-Xmx2g"])))
  (testing "a ceiling below the initial-heap cap sets the initial heap to the ceiling"
    (is (= {:ok ["-J-Xmx256m" "-J-XX:SoftMaxHeapSize=256m" "-J-Xms256m"]}
           (heap/pair-heap-args ["-J-Xmx256m"])))))

(deftest explicit-values-are-kept
  (is (= {:ok ["-Srepro" "-J-Xmx2g" "-J-XX:SoftMaxHeapSize=1g" "-J-Xms1g"]}
         (heap/pair-heap-args ["-Srepro" "-J-Xmx2g" "-J-XX:SoftMaxHeapSize=1g" "-J-Xms1g"]))))

(deftest no-xmx-means-no-change
  (is (= {:ok ["-Srepro"]} (heap/pair-heap-args ["-Srepro"])))
  (is (= {:ok nil} (heap/pair-heap-args nil))))

(deftest contradictory-explicit-values-are-refused
  (let [r (heap/pair-heap-args ["-J-Xmx2g" "-J-XX:SoftMaxHeapSize=4g"])]
    (is (:error r))
    (is (str/includes? (:error r) "SoftMaxHeapSize"))
    (is (str/includes? (:error r) "-Xmx2g")))
  (is (:error (heap/pair-heap-args ["-J-Xmx1g" "-J-Xms2g"]))))

(deftest the-last-xmx-wins
  (is (= {:ok ["-J-Xmx6g" "-J-Xmx1g" "-J-XX:SoftMaxHeapSize=1g" "-J-Xms512m"]}
         (heap/pair-heap-args ["-J-Xmx6g" "-J-Xmx1g"]))))

(defn- ok-stub []
  (let [calls (atom [])]
    {:calls calls
     :eval-fn (fn ([code] (swap! calls conj code) {:success true :result "\"{}\""})
                ([code _] (swap! calls conj code) {:success true :result "\"{}\""}))}))

(deftest spawn-pairs-the-soft-heap-with-a-lower-xmx
  (let [{:keys [calls eval-fn]} (ok-stub)]
    (spawn/reset-watches!)
    (try
      (binding [cider/*eval-fn* eval-fn spawn-dir/*directory?* #{"/p"}]
        (cider/handle-spawn {:name "small" :project_dir "/p" :extra_args ["-J-Xmx2g"]})
        (is (str/includes? (first @calls)
                           ":extra-args '(\"-J-Xmx2g\" \"-J-XX:SoftMaxHeapSize=2g\" \"-J-Xms512m\")")))
      (finally (spawn/reset-watches!)))))

(deftest spawn-refuses-a-contradiction-before-any-jvm-starts
  (let [{:keys [calls eval-fn]} (ok-stub)]
    (binding [cider/*eval-fn* eval-fn spawn-dir/*directory?* #{"/p"}]
      (let [resp (cider/handle-spawn {:name "bad" :project_dir "/p"
                                      :extra_args ["-J-Xmx2g" "-J-XX:SoftMaxHeapSize=4g"]})]
        (is (:isError resp))
        (is (str/includes? (pr-str resp) "SoftMaxHeapSize"))
        (is (empty? @calls))))))
