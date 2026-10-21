(ns hive-emacs.attention-render
  "Pure rendering policy for the attention channel: which waiting prompt
   impedes agents and the paragraph that tells an agent what to do about it.
   Data in, text out; no I/O."
  (:require [clojure.string :as str]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def user-origin
  "Origin of a prompt the human opened with a key. Never an impediment."
  "command")

(defn actionable?
  "True when this prompt impedes agents: not one the human opened with a key."
  [{:keys [origin]}]
  (not= user-origin origin))

(defn- waited
  [{:keys [since-ms]} now-ms]
  (if since-ms
    (str (max 0 (quot (- now-ms since-ms) 1000)) "s")
    "?s"))

(defn- quoted
  [s]
  (pr-str (str/trim (str/replace (or s "") #"\s+" " "))))

(defn- opened-by
  [origin]
  (case origin
    "server" "an emacsclient eval"
    "timer" "a timer"
    "async" "a process filter or sentinel (e.g. a CIDER connection callback)"
    "command" "the user"
    "an unknown caller"))

(defn- what
  [{:keys [kind prompt]}]
  (case kind
    "debugger" (str "the Emacs DEBUGGER is open: "
                    (quoted (first (str/split-lines (or prompt "")))))
    "read-event" (str "a key prompt " (quoted prompt))
    (str "a minibuffer prompt " (quoted prompt))))

(defn- consequence
  [{:keys [origin]} status]
  (cond
    (= :frozen status)
    "Its timer has STOPPED, so an answer file will not be read either: Emacs is busy in a blocking loop. Ask the user."
    (= "server" origin)
    "Every emacsclient call (emacs, cider, magit tools) times out until it is answered."
    :else
    "Work waiting on it (a spawn, a connection, a load) cannot finish until it is answered."))

(defn- question-kind
  "What a prompt asks for, read from its text: :yes-or-no (typed word),
   :y-or-n (one key) or :text. The keys to suggest follow from it: \"y\" is a
   wrong answer to a completing-read such as \"REPL buffer to reuse:\"."
  [prompt]
  (cond
    (re-find #"\((?:yes or no)\)" (or prompt "")) :yes-or-no
    (re-find #"(?i)\(y or n\)|[\[(]y/n[\])]" (or prompt "")) :y-or-n
    :else :text))

(defn- action
  [{:keys [id kind secret? prompt]} status]
  (cond
    (= :frozen status) nil
    secret? "SECRET prompt (password): do not answer it. Ask the user to type it in Emacs."
    (= "debugger" kind)
    (format "Act: emacs command=\"answer\" id=\"%s\" keys=\"q\" quits the debugger, or ask the user."
            id)
    :else
    (case (question-kind prompt)
      :yes-or-no
      (format "Act: emacs command=\"answer\" id=\"%s\" keys=\"yes RET\" (or \"no RET\"; \"C-g\" aborts), or ask the user."
              id)
      :y-or-n
      (format "Act: emacs command=\"answer\" id=\"%s\" keys=\"y\" (or \"n\"; \"C-g\" aborts), or ask the user."
              id)
      :text
      (if (= "read-event" kind)
        (format "Act: emacs command=\"answer\" id=\"%s\" keys=\"<a key the prompt offers>\" (kbd syntax, \"C-g\" aborts), or ask the user."
                id)
        (format "Act: emacs command=\"answer\" id=\"%s\" keys=\"RET\" accepts the default, keys=\"<text> RET\" types a value (kbd syntax: SPC is a space, \"C-g\" aborts), or ask the user."
                id)))))

(defn describe
  "One paragraph for one waiting prompt."
  [state status now-ms]
  (->> [(format "Emacs daemon %s is WAITING FOR INPUT (%s, %s): %s, opened by %s."
                (quoted (:server state))
                (waited state now-ms)
                (name status)
                (what state)
                (opened-by (:origin state)))
        (consequence state status)
        (action state status)]
       (remove nil?)
       (str/join "\n")))

(defn render-block
  "Block body for `[[state status] ...]`, or nil when nothing impedes agents."
  [states+status now-ms]
  (let [paragraphs (for [[state status] states+status
                         :when (and (actionable? state) (not= :gone status))]
                     (describe state status now-ms))]
    (when (seq paragraphs)
      (str/join "\n\n" paragraphs))))
