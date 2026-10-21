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
            [hive-test.trifecta :refer [deftrifecta]]
            [clojure.edn :as edn]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defn compose-case
  "compose over CASE's declared aliases, explicit aliases and repl type. A case
   carrying :deps-text declares what `declared-aliases` reads from that text."
  [{:keys [declared deps-text explicit repl-type]}]
  (ac/compose (if deps-text (ac/declared-aliases deps-text) declared) explicit repl-type))

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
           :cljel-composed     {:declared #{"dev"} :explicit nil :repl-type "cljel"}
           ;; from deps.edn text: aliases that declare :main-opts are never composed
           :text-dev-extra-paths-kept
           {:deps-text "{:aliases {:dev {:extra-paths [\"dev\"]}}}" :explicit nil :repl-type "clj"}
           :text-test-kaocha-dropped
           {:deps-text "{:aliases {:dev {:extra-paths [\"dev\"]} :test {:extra-paths [\"test\"] :main-opts [\"-m\" \"kaocha.runner\"]}}}"
            :explicit nil :repl-type "clj"}
           :text-nrepl-main-opts-dropped
           {:deps-text "{:aliases {:dev {} :test {:extra-paths [\"test\"]} :nrepl {:main-opts [\"-m\" \"nrepl.cmdline\" \"--port\" \"7950\"]}}}"
            :explicit nil :repl-type "clj"}
           :text-only-launchers
           {:deps-text "{:aliases {:test {:main-opts [\"-m\" \"cognitect.test-runner\"]} :nrepl {:main-opts [\"-m\" \"nrepl.cmdline\"]}}}"
            :explicit nil :repl-type "clj"}}
   :gen case-gen
   :pred (fn [r] (or (nil? r)
                     (and (vector? r) (seq r) (= r (distinct r)))))
   :num-tests 100
   :mutations [["ignores-declared"
                (fn [{:keys [explicit]}]
                  (if (seq explicit) (ac/normalize explicit) ["dev" "test" "nrepl"]))]
               ["declaration-order"
                (fn [{:keys [declared explicit]}]
                  (if (seq explicit) (ac/normalize explicit) (not-empty (vec (sort declared)))))]
               ["composes-launchers"
                (fn [{:keys [declared deps-text explicit repl-type]}]
                  (let [every (if deps-text
                                (->> (re-seq #":(dev|test|nrepl)\b" deps-text) (map second) set)
                                declared)]
                    (ac/compose every explicit repl-type)))]]})

(defn declared-case
  [text]
  (vec (sort (ac/declared-aliases text))))

(deftrifecta declared-aliases
  hive-emacs.cider.alias-compose-test/declared-case
  {:golden-path "test/golden/hive_emacs/cider/alias_compose_declared.edn"
   :cases {:typical  "{:paths [\"src\"] :aliases {:dev {:extra-paths [\"dev\"]} :test {} :nrepl {}}}"
           :dev-extra-paths-kept "{:aliases {:dev {:extra-paths [\"dev\" \"test\"]}}}"
           :test-kaocha-dropped "{:aliases {:dev {:extra-paths [\"dev\"]} :test {:extra-paths [\"test\"] :main-opts [\"-m\" \"kaocha.runner\"]}}}"
           :test-cognitect-dropped "{:aliases {:test {:main-opts [\"-m\" \"cognitect.test-runner\"]}}}"
           :nrepl-main-opts-dropped "{:aliases {:dev {} :nrepl {:main-opts [\"-m\" \"nrepl.cmdline\" \"--port\" \"7950\"]}}}"
           :exec-fn-dropped "{:aliases {:dev {} :build {:exec-fn build/jar}}}"
           :empty-main-opts-kept "{:aliases {:dev {:main-opts []}}}"
           :none     "{:paths [\"src\"]}"
           :blank    ""
           :nil      nil
           :garbage  "{:aliases {:dev"
           :not-map  "[:aliases]"}
   :gen (gen/elements ["{:aliases {:a {}}}"
                       "{:aliases {:a {:main-opts [\"-m\" \"x\"]}}}"
                       "" "{" "{:aliases 3}" nil])
   :pred (fn [r] (and (vector? r) (every? string? r)))
   :num-tests 50
   :mutations [["keeps-colons"
                (fn [text] (vec (sort (map #(str ":" %) (ac/declared-aliases text)))))]
               ["keeps-launchers"
                (fn [text]
                  (let [m (when-not (str/blank? text)
                            (try (clojure.edn/read-string {:default (fn [_ v] v)} text)
                                 (catch Exception _ nil)))
                        a (when (map? m) (:aliases m))]
                    (if (map? a) (vec (sort (map name (keys a)))) [])))]]})

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

(deftest spawn-drops-aliases-that-declare-main-opts
  (let [form (spawn-form (str "{:aliases {:dev {:extra-paths [\"dev\"]}"
                              " :test {:main-opts [\"-m\" \"kaocha.runner\"]}"
                              " :nrepl {:main-opts [\"-m\" \"nrepl.cmdline\" \"--port\" \"7950\"]}}}")
                         {})]
    (is (str/includes? form ":aliases '(\"dev\")") form)
    (is (not (str/includes? form "kaocha")) form)))

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

(defn- auto-spawn-form
  "The elisp the auto-connect spawn (spawn-session-internal) sends for
   PROJECT-DIR over a project whose deps.edn is TEXT."
  [text project-dir]
  (let [calls (atom [])]
    (binding [cider/*eval-fn* (recording-eval calls)
              ac/*read-deps-fn* (constantly text)]
      (#'cider/spawn-session-internal "auto-x" project-dir))
    (first @calls)))

(deftest auto-spawn-composes-the-declared-aliases
  (let [form (auto-spawn-form "{:aliases {:test {} :dev {} :other {}}}" dir)]
    (is (str/includes? form ":aliases '(\"dev\" \"test\")") form)
    (is (str/includes? form "-J-Xmx2g") form)))

(deftest auto-spawn-without-project-dir-composes-nothing
  (let [form (auto-spawn-form "{:aliases {:test {} :dev {}}}" nil)]
    (is (not (str/includes? form ":aliases")) form)))
