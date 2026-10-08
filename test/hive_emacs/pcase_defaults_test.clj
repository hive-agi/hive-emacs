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
  (let [n (count source)
        forms (loop [i 0 token "" stack [{:children []}] quoted? false escaped? false comment? false]
                (if (= i n)
                  (:children (first stack))
                  (let [c (.charAt ^String source i)
                        flush-token (fn [frames token]
                                      (if (empty? token) frames
                                          (update-in frames [(dec (count frames)) :children]
                                                     conj token)))]
                    (cond
                      comment? (recur (inc i) token stack false false (not= c \newline))
                      quoted? (recur (inc i) token stack
                                     (or escaped? (= c \\) (not= c \"))
                                     (and (not escaped?) (= c \\)) false)
                      (= c \;) (recur (inc i) "" (flush-token stack token) false false true)
                      (= c \") (recur (inc i) "" (flush-token stack token) true false false)
                      (#{\( \[ \{} c)
                      (recur (inc i) "" (conj (flush-token stack token)
                                               {:delimiter c :children []}) false false false)
                      (#{\) \] \}} c)
                      (let [frames (flush-token stack token)
                            closed (peek frames)]
                        (recur (inc i) "" (update-in (pop frames)
                                                      [(- (count frames) 2) :children]
                                                      conj closed) false false false))
                      (or (Character/isWhitespace c) (= c \,))
                      (recur (inc i) "" (flush-token stack token) false false false)
                      :else (recur (inc i) (str token c) stack false false false)))))]
    (boolean
     (some (fn [form]
             (and (= \( (:delimiter form))
                  (= "pcase" (first (:children form)))
                  (some (fn [clause]
                          (and (= \( (:delimiter clause))
                               (= "_" (first (:children clause)))))
                        (drop 2 (:children form)))))
           (tree-seq map? :children {:children forms})))))

(defn- no-literal-wildcard-default? [source]
  (not (bare-wildcard? source)))

(deftrifecta no-literal-wildcard-default
  hive-emacs.pcase-defaults-test/no-literal-wildcard-default?
  {:golden-path "test/golden/hive_emacs/pcase_defaults/no_literal_wildcard_default.edn"
   :cases {:legacy "(pcase backend ('eat 1) (_ (error \"unsupported\")))"
           :fixed "(elisp-cond ((equal backend 'eat) 1) (t (error \"unsupported\")))"
           :nil-default "(pcase backend ('eat 1) (_ nil))"
           :no-default "(pcase backend ('eat 1))"
           :unrelated-underscore "(dotimes (_ 3) nil) (pcase backend ('eat 1))"
           :nested-default "(let [result (pcase backend ('eat 1) (_ nil))] result)"
           :quoted-text "(str \"(pcase backend (_ nil))\")"
           :qualified-underscore "(pcase backend (other/_ nil))"}
   :gen (gen/elements ["(pcase backend ('eat 1) (_ nil))"
                       "(pcase backend ('eat 1))"
                       "(elisp-cond ((equal backend 'eat) 1) (t nil))"
                       "(dotimes (_ 3) nil) (pcase backend ('eat 1))"])
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
