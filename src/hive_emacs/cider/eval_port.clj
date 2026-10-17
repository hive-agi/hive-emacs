(ns hive-emacs.cider.eval-port
  "Which nREPL a CIDER eval names, and how the session registry answers for it.

   Pure except for the port-file boundary `*port-file-fn*`. The eval handler
   in hive-emacs.tools.cider routes on `resolve-eval-port`: a named port is
   connected to and never spawned beside, so a worktree that already runs an
   nREPL never gets a second JVM."
  (:require [clojure.string :as str]
            [hive-dsl.result :as result]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def ^:dynamic *port-file-fn*
  "Port-file boundary: (f directory) -> the text of DIRECTORY's .nrepl-port,
   or nil when it has none. Tests bind a stub."
  (fn [directory]
    (let [f (java.io.File. (str directory) ".nrepl-port")]
      (when (.isFile f)
        (slurp f)))))

(def order
  "Where eval looks for its nREPL, in order. Every port error names it."
  (str "1. the port param, 2. <directory>/.nrepl-port, "
       "3. the directory's auto-<hash> session, spawned when none is connected"))

(defn parse-port
  "V as a TCP port number, or nil when it is not one."
  [v]
  (let [n (cond (integer? v) (long v)
                (string? v)  (parse-long (str/trim v))
                :else        nil)]
    (when (and n (< 0 n 65536)) n)))

(defn resolve-port
  "The nREPL an eval names, as a Result: (ok {:port n :source s}) from the
   port param (its :port_source says where the caller got it) or from
   DIRECTORY's .nrepl-port; (ok nil) when neither names one; an err when one
   is named but is not a port."
  [params directory]
  (let [given (:port params)
        bad   (fn [what]
                (result/err :cider/bad-port
                            {:message (str what ", not a TCP port (1-65535). Resolution order: "
                                           order)}))]
    (cond
      (some? given)
      (if-let [n (parse-port given)]
        (result/ok {:port n :source (or (not-empty (str (:port_source params))) "the port param")})
        (bad (str "the port param is " (pr-str given))))

      (str/blank? directory)
      (result/ok nil)

      :else
      (let [file (.getPath (java.io.File. (str directory) ".nrepl-port"))
            text (result/rescue nil (*port-file-fn* directory))]
        (cond
          (nil? text)       (result/ok nil)
          (parse-port text) (result/ok {:port (parse-port text) :source file})
          :else             (bad (str file " holds " (pr-str (str/trim text)))))))))

(defn session-on-port
  "Name of a connected session whose nREPL listens on PORT, or nil."
  [sessions port]
  (some (fn [s]
          (when (and (= "connected" (:status s))
                     (= port (parse-port (:port s))))
            (:name s)))
        sessions))

(defn session-state
  "How the registry describes session NAME, for an error message."
  [sessions name]
  (if-let [s (first (filter #(= name (:name %)) sessions))]
    (str (:status s) (when-let [why (not-empty (str (:reason s)))] (str " (" why ")")))
    "not registered"))

(defn unreachable
  "The err for PORT, taken from SOURCE, that did not give a connected session."
  [{:keys [port source]} why]
  (result/err :cider/port-unreachable
              {:message (str "no nREPL answered on port " port " (taken from " source
                             "): " why ". Nothing was spawned. Start that nREPL, correct "
                             source ", or pass port. Resolution order: " order)}))
