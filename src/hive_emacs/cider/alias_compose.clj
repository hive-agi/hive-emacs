(ns hive-emacs.cider.alias-compose
  "Which deps.edn aliases a `cider spawn` launches with, decided from the
   aliases the target project actually declares.

   The Emacs half builds `clojure -Sdeps ... -M<aliases> -m nrepl.cmdline`.
   With no aliases the REPL has none of the project's :dev or :test
   extra-paths, so `test/` is not on its classpath. This namespace composes
   the aliases to hand it, in the order :dev, :test, :nrepl, keeping only
   those the project declares, so a project without a :nrepl alias still
   launches.

   Layers: compose, declared-aliases and normalize are pure; read-deps-text
   is the one effect, behind the *read-deps-fn* seam."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [hive-dsl.result :as result]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def composable-order
  "The aliases a spawn composes when the project declares them, in launch order."
  ["dev" "test" "nrepl"])

(def opt-out
  "The aliases value that asks for a plain -M launch: nothing is composed."
  ["none"])

(def composing-repl-types
  "REPL types launched through the clojure CLI, where -M aliases apply."
  #{"clj" "cljel"})

(defn normalize
  "Alias names as distinct bare strings in first-seen order: a leading colon is
   stripped, blank entries dropped. Pure."
  [aliases]
  (->> aliases
       (map (fn [a] (let [s (str/trim (str (if (keyword? a) (name a) a)))]
                      (if (str/starts-with? s ":") (subs s 1) s))))
       (remove str/blank?)
       distinct
       vec))

(defn declared-aliases
  "The set of bare alias names declared under :aliases in DEPS-TEXT, the text of
   a deps.edn. Empty for nil, blank or unreadable text. Never throws. Pure."
  [deps-text]
  (let [parsed (when-not (str/blank? deps-text)
                 (result/rescue nil (edn/read-string {:default (fn [_ v] v)} deps-text)))
        aliases (when (map? parsed) (:aliases parsed))]
    (if (map? aliases)
      (into #{} (comp (filter #(or (keyword? %) (symbol? %))) (map name)) (keys aliases))
      #{})))

(defn compose
  "The aliases to launch REPL-TYPE with. EXPLICIT (aliases the caller named)
   wins, normalized. `opt-out` yields nil, a plain -M launch. With none named,
   the members of `composable-order` that DECLARED holds, in that order; nil
   when none are declared or REPL-TYPE does not launch through the clojure CLI
   (a nil REPL-TYPE means clj). Pure."
  [declared explicit repl-type]
  (let [wanted (normalize explicit)]
    (cond
      (= opt-out wanted) nil
      (seq wanted) wanted
      (not (contains? composing-repl-types (or repl-type "clj"))) nil
      :else (not-empty (filterv (set declared) composable-order)))))

(defn read-deps-text
  "The text of DIR's deps.edn, or nil when DIR is nil or has none."
  [dir]
  (when dir
    (let [f (io/file dir "deps.edn")]
      (when (.isFile f) (slurp f)))))

(def ^:dynamic *read-deps-fn*
  "Project-probe boundary: (f dir) -> deps.edn text or nil. Read per call;
   tests bind a stub."
  read-deps-text)

(defn for-project
  "The aliases a spawn in DIR launches with: `compose` over the aliases DIR's
   deps.edn declares. A probe that throws counts as no deps.edn."
  [dir explicit repl-type]
  (let [text (result/rescue nil (*read-deps-fn* dir))]
    (compose (declared-aliases text) explicit repl-type)))
