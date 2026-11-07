(ns hive-emacs.cider.heap
  "Heap flags of a spawn's extra_args, made consistent before the JVM starts.

   A project alias can carry its own -XX:SoftMaxHeapSize and -Xms. A caller
   that lowers only -Xmx then gets a JVM that refuses to boot. When extra_args
   name an -Xmx, the soft heap and the initial heap are pinned at or below it;
   explicit values that contradict the -Xmx are refused with a message."
  (:require [clojure.string :as str]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def initial-heap-cap
  "The largest -Xms a paired spawn is given when the caller names none."
  "512m")

(defn parse-size
  "Bytes in a JVM size like \"2g\", \"512m\", \"64k\" or \"1000\"; nil if unreadable."
  [s]
  (when-let [[_ n unit] (and (string? s) (re-matches #"(?i)(\d+)([kmgt]?)" s))]
    (* (parse-long n)
       (case (str/lower-case unit)
         "" 1
         "k" 1024
         "m" (* 1024 1024)
         "g" (* 1024 1024 1024)
         "t" (* 1024 1024 1024 1024)))))

(defn- last-flag
  "The value of the last arg in ARGS matching RE (one capture group)."
  [re args]
  (some->> args (keep #(second (re-matches re %))) last))

(def ^:private xmx-re #"-J-Xmx(\S+)")
(def ^:private xms-re #"-J-Xms(\S+)")
(def ^:private soft-re #"-J-XX:SoftMaxHeapSize=(\S+)")

(defn- smaller [a b]
  (if (<= (parse-size a) (parse-size b)) a b))

(defn pair-heap-args
  "Make the heap flags in ARGS (a spawn's extra_args) consistent.
   {:ok args'} where args' appends a SoftMaxHeapSize and an -Xms when an -Xmx
   is present and they are missing, or {:error msg} when an explicit one is
   larger than the -Xmx. ARGS without an -Xmx pass through unchanged."
  [args]
  (let [xmx (last-flag xmx-re args)]
    (if-not (and xmx (parse-size xmx))
      {:ok args}
      (let [soft (last-flag soft-re args)
            xms (last-flag xms-re args)
            too-big (fn [flag v]
                      (when (and v (parse-size v) (> (parse-size v) (parse-size xmx)))
                        (str flag v " is larger than -Xmx" xmx
                             ", so the JVM would refuse to start. Lower " flag
                             " to at most " xmx " or drop it and let spawn pair it with -Xmx.")))]
        (if-let [msg (or (too-big "-XX:SoftMaxHeapSize=" soft) (too-big "-Xms" xms))]
          {:error (str "Error: spawn refused before any JVM started: " msg)}
          {:ok (cond-> (vec args)
                 (nil? soft) (conj (str "-J-XX:SoftMaxHeapSize=" xmx))
                 (nil? xms) (conj (str "-J-Xms" (smaller initial-heap-cap xmx))))})))))
