(ns hive-emacs.cider.heap-cap
  "The heap ceiling a `cider spawn` JVM launches with.

   A JVM with no -Xmx grows toward a quarter of host RAM. A spawn names one:
   the caller's own `-J-Xmx` in extra_args, else a selected alias's
   :jvm-opts -Xmx, else `default-heap`. A bare `-Xmx`/`-Xms` in extra_args is
   given the `-J` the clojure CLI needs to pass it to the JVM.

   Layers: everything but `for-project` is pure; it reads deps.edn through
   alias-compose's *read-deps-fn* seam."
  (:require [clojure.edn :as edn]
            [hive-dsl.result :as result]
            [hive-emacs.cider.alias-compose :as alias-compose]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def default-heap
  "The -Xmx a spawn gets when neither the caller nor its aliases name one."
  "-J-Xmx2g")

(def jvm-repl-types
  "REPL types launched through the clojure CLI, where -J opts reach the JVM."
  #{"clj" "cljel"})

(defn- xmx? [s] (and (string? s) (.startsWith ^String s "-Xmx")))

(defn- jvm-flag
  "A bare -Xmx/-Xms given the -J prefix the clojure CLI requires."
  [arg]
  (if (and (string? arg) (or (.startsWith ^String arg "-Xmx") (.startsWith ^String arg "-Xms")))
    (str "-J" arg)
    arg))

(defn alias-capped?
  "True when one of ALIASES (bare names) in parsed DEPS (a deps.edn map)
   carries an -Xmx in its :jvm-opts."
  [deps aliases]
  (let [defs (when (map? deps) (:aliases deps))]
    (boolean
     (and (map? defs)
          (some (fn [a] (some xmx? (:jvm-opts (get defs (keyword a)))))
                aliases)))))

(defn with-heap-cap
  "EXTRA-ARGS for a spawn of REPL-TYPE (nil means clj), with a heap ceiling.
   Non-JVM REPL types pass through untouched. Bare -Xmx/-Xms become -J opts;
   `default-heap` is appended unless an arg already names -J-Xmx or
   ALIAS-CAPPED? holds. Answers a vector, or nil when it would be empty."
  [repl-type extra-args alias-capped?]
  (if-not (contains? jvm-repl-types (or repl-type "clj"))
    extra-args
    (let [args (mapv jvm-flag extra-args)
          capped? (or alias-capped?
                      (some #(and (string? %) (.startsWith ^String % "-J-Xmx")) args))]
      (not-empty (cond-> args (not capped?) (conj default-heap))))))

(defn for-project
  "EXTRA-ARGS with a heap ceiling for a spawn of REPL-TYPE in DIR launched
   with ALIASES: `with-heap-cap`, where an alias cap is read from DIR's
   deps.edn. An unreadable deps.edn counts as no alias cap."
  [dir repl-type extra-args aliases]
  (let [deps (result/rescue nil
               (some->> (alias-compose/*read-deps-fn* dir)
                        (edn/read-string {:default (fn [_ v] v)})))]
    (with-heap-cap repl-type extra-args (alias-capped? deps aliases))))
