(ns hive-emacs.cider.alias-compose-test
  "A spawn with no aliases named launches with the :dev, :test and :nrepl
   aliases the project declares, in that order; named aliases win; [\"none\"]
   asks for a plain launch. The deps.edn probe is a stub; the spawn path's
   elisp boundary is a recording *eval-fn*."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [hive-emacs.cider.alias-compose :as ac]
            [hive-emacs.tools.cider :as cider]
            [hive-test.trifecta :refer [deftrifecta]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defn compose-case
  "compose over CASE's declared aliases, explicit aliases and repl type."
  [{:keys [declared explicit repl-type]}]
  (ac/compose declared explicit repl-type))

(def case-gen
  (gen/let [declared (gen/set (gen/elements ["dev" "test" "nrepl" "build" "typed"]))
            explicit (gen/vector (gen/elements ["" ":dev" "perf" "none" "dev"]) 0 3)
            repl-type (gen/elements [nil "clj" "cljs" "cljel" "cljw"])]
    {:declared declared :explicit explicit :repl-type repl-type}))

(deftrifecta compose
  hive-emacs.cider.alias-compose-test/compose-case
  {:golden-path "test/golden/hive_emacs/cider/alias_compose.edn"
   :cases {:dev-test-nrepl     {:declared #{"nrepl" "test" "dev"} :explicit nil :repl-type "clj"}
           :no-nrepl-alias     {:declared #{"test" "dev"} :explicit nil :repl-type nil}
           :test-only          {:declared #{"test" "build"} :explicit [] :repl-type "clj"}
           :none-declared      {:declared #{"build"} :explicit nil :repl-type "clj"}
           :explicit-wins      {:declared #{"dev" "test"} :explicit [":perf"] :repl-type "clj"}
           :explicit-dedup     {:declared #{} :explicit ["dev" ":dev" ""] :repl-type "clj"}
           :opt-out            {:declared #{"dev" "test"} :explicit ["none"] :repl-type "clj"}
           :cljs-not-composed  {:declared #{"dev" "test"} :explicit nil :repl-type "cljs"}
           :cljel-composed     {:declared #{"dev"} :explicit nil :repl-type "cljel"}}
   :gen case-gen
   :pred (fn [r] (or (nil? r)
                     (and (vector? r) (seq r) (= r (distinct r)))))
   :num-tests 100
   :mutations [["ignores-declared"
                (fn [{:keys [explicit]}]
                  (if (seq explicit) (ac/normalize explicit) ["dev" "test" "nrepl"]))]
               ["declaration-order"
                (fn [{:keys [declared explicit]}]
                  (if (seq explicit) (ac/normalize explicit) (not-empty (vec (sort declared)))))]]})

(defn declared-case
  [text]
  (vec (sort (ac/declared-aliases text))))

(deftrifecta declared-aliases
  hive-emacs.cider.alias-compose-test/declared-case
  {:golden-path "test/golden/hive_emacs/cider/alias_compose_declared.edn"
   :cases {:typical  "{:paths [\"src\"] :aliases {:dev {:extra-paths [\"dev\"]} :test {} :nrepl {}}}"
           :none     "{:paths [\"src\"]}"
           :blank    ""
           :nil      nil
           :garbage  "{:aliases {:dev"
           :not-map  "[:aliases]"}
   :gen (gen/elements ["{:aliases {:a {}}}" "" "{" "{:aliases 3}" nil])
   :pred (fn [r] (and (vector? r) (every? string? r)))
   :num-tests 50
   :mutations [["keeps-colons"
                (fn [text] (vec (sort (map #(str ":" %) (ac/declared-aliases text)))))]]})

(defn- recording-eval [calls]
  (fn
    ([code] (swap! calls conj code) {:success true :result "\"{}\""})
    ([code _timeout-ms] (swap! calls conj code) {:success true :result "\"{}\""})))

(def dir (System/getProperty "java.io.tmpdir"))

(defn- spawn-form
  "The elisp handle-spawn sends for PARAMS over a project whose deps.edn is TEXT."
  [text params]
  (let [calls (atom [])]
    (binding [cider/*eval-fn* (recording-eval calls)
              ac/*read-deps-fn* (constantly text)]
      (cider/handle-spawn (merge {:name "s" :project_dir dir} params)))
    (first @calls)))

(deftest spawn-composes-the-declared-aliases
  (let [form (spawn-form "{:aliases {:test {} :dev {} :other {}}}" {})]
    (is (str/includes? form ":aliases '(\"dev\" \"test\")") form)))

(deftest spawn-named-aliases-win
  (let [form (spawn-form "{:aliases {:test {} :dev {}}}" {:aliases "perf"})]
    (is (str/includes? form ":aliases '(\"perf\")") form)))

(deftest spawn-none-is-a-plain-launch
  (let [form (spawn-form "{:aliases {:test {} :dev {}}}" {:aliases "none"})]
    (is (not (str/includes? form ":aliases")) form)))

(deftest spawn-without-a-deps-edn-composes-nothing
  (let [form (spawn-form nil {})]
    (is (not (str/includes? form ":aliases")) form)))

(deftest spawn-cljs-is-not-composed
  (let [form (spawn-form "{:aliases {:dev {}}}" {:repl_type "cljs"})]
    (is (not (str/includes? form ":aliases")) form)))
