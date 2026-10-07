(ns hive-emacs.cider.spawn-dir-test
  "A spawn runs in project_dir, else directory; a named path that is not a
   directory is refused before Emacs is asked. The directory check is a
   stub set; the spawn path's elisp boundary is a recording *eval-fn*."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [hive-emacs.cider.spawn-dir :as spawn-dir]
            [hive-emacs.tools.cider :as cider]
            [hive-test.trifecta :refer [deftrifecta]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def known-dirs #{"/repo/a" "/repo/b"})

(defn resolve-case
  "resolve-dir over PARAMS with KNOWN-DIRS as the only directories:
   the chosen path, or the err category."
  [params]
  (let [r (spawn-dir/resolve-dir params known-dirs)]
    (if (contains? r :ok) [:ok (:ok r)] [:err (:error r)])))

(def param-gen
  (gen/let [pd (gen/elements [nil "" "/repo/a" "/missing"])
            d  (gen/elements [nil "  " "/repo/b" "/gone"])]
    (cond-> {}
      pd (assoc :project_dir pd)
      d  (assoc :directory d))))

(deftrifecta resolve-dir
  hive-emacs.cider.spawn-dir-test/resolve-case
  {:golden-path "test/golden/hive_emacs/cider/spawn_dir_resolve.edn"
   :cases {:neither              {}
           :project-dir          {:project_dir "/repo/a"}
           :directory-only       {:directory "/repo/b"}
           :project-dir-wins     {:project_dir "/repo/a" :directory "/repo/b"}
           :blank-project-dir    {:project_dir "" :directory "/repo/b"}
           :missing-directory    {:directory "/gone"}
           :missing-project-dir  {:project_dir "/missing" :directory "/repo/b"}}
   :gen param-gen
   :pred (fn [[tag v]] (case tag :ok (or (nil? v) (contains? known-dirs v)) :err true))
   :num-tests 100
   :mutations [["directory-ignored"
                (fn [{:keys [project_dir]}]
                  (cond (str/blank? project_dir)        [:ok nil]
                        (known-dirs project_dir)        [:ok project_dir]
                        :else                           [:err :cider/bad-project-dir]))]
               ["never-refuses"
                (fn [{:keys [project_dir directory]}]
                  [:ok (first (remove str/blank? [project_dir directory]))])]]})

(defn- recording-eval [calls]
  (fn
    ([code] (swap! calls conj code) {:success true :result "\"{}\""})
    ([code _timeout-ms] (swap! calls conj code) {:success true :result "\"{}\""})))

(deftest spawn-runs-in-directory-when-project-dir-is-absent
  (let [calls (atom [])
        dir   (System/getProperty "java.io.tmpdir")]
    (binding [cider/*eval-fn* (recording-eval calls)]
      (cider/handle-spawn {:name "s" :directory dir})
      (is (str/includes? (first @calls) (str ":project-dir " (pr-str dir))) (first @calls)))))

(deftest spawn-refuses-a-directory-that-does-not-exist
  (let [calls (atom [])]
    (binding [cider/*eval-fn* (recording-eval calls)]
      (let [out (cider/handle-spawn {:name "s" :directory "/no/such/dir/for/spawn"})]
        (is (:isError out) (pr-str out))
        (is (empty? @calls) "Emacs is never asked")))))
