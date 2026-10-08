(ns hive-emacs.pcase-defaults-test
  "Source-level regression for cljel pcase's literal `_` default bug.
   The dispatch contract is pure and checks the complete migrated source set;
   the pinned compiler is checked independently by the bb CLI."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]))

(defn- bare-wildcard?
  "Detect a bare wildcard clause in pcase source (including multiline bodies)."
  [source]
  (boolean (and (str/includes? source "(pcase ")
                (str/includes? source "(_ "))))

(defn- no-literal-wildcard-default? [source]
  (not (bare-wildcard? source)))

(deftrifecta no-literal-wildcard-default
  hive-emacs.pcase-defaults-test/no-literal-wildcard-default?
  {:golden-path "test/golden/hive_emacs/pcase_defaults/no_literal_wildcard_default.edn"
   :cases {:legacy "(pcase backend ('eat 1) (_ (error \"unsupported\")))"
           :fixed "(elisp-cond ((equal backend 'eat) 1) (t (error \"unsupported\")))"
           :nil-default "(pcase backend ('eat 1) (_ nil))"
           :no-default "(pcase backend ('eat 1))"}
   :gen (gen/elements ["(pcase backend ('eat 1) (_ nil))"
                       "(pcase backend ('eat 1))"
                       "(elisp-cond ((equal backend 'eat) 1) (t nil))"])
   :pred boolean?
   :num-tests 30
   :mutations [["always-valid" (fn [_] true)]
               ["always-invalid" (fn [_] false)]]})

(defn- changed-source-paths []
  (let [root (io/file "src/cljel")]
    (->> (file-seq root)
         (filter #(.isFile ^java.io.File %))
         (filter #(str/ends-with? (.getName ^java.io.File %) ".cljel")))))

(deftest no-source-dispatch-uses-a-literal-default
  (doseq [source (changed-source-paths)]
    (is (not (bare-wildcard? (slurp source))) (.getPath source))))
