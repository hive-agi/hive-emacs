(ns hive-emacs.editor-adapter
  "Host-neutral Emacs editor capability.

   The returned map is data plus functions. A host-specific adapter may wrap it
   in its own editor protocol; hive-emacs never imports or resolves that host."
  (:require [clojure.string :as str]
            [hive-dsl.result :as result]
            [hive-emacs.client :as ec]
            [hive-emacs.runtime-ports :as ports]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defn- ec-result->result
  [{:keys [success result error timed-out]}]
  (cond
    success (result/ok result)
    timed-out (result/err :editor/timeout {:message error})
    :else (result/err :editor/eval-failed {:message error})))

(defn- feature-available?
  [feature-name]
  (try
    (let [{:keys [success result]}
          (ec/eval-elisp (format "(featurep '%s)" feature-name))]
      (and success (= "t" (str/trim (or result "")))))
    (catch Exception _ false)))

(defn- send-to-terminal
  [terminal-id text]
  (try
    (let [response (ports/dispatch-terminal! terminal-id text)]
      (cond
        (result/err? response) response
        (false? (:success? response))
        (result/err :editor/no-terminal
                    {:message (or (:message response)
                                  (some-> (:error response) str))})
        :else (result/ok true)))
    (catch Exception e
      (result/err :editor/dispatch-failed {:message (ex-message e)}))))

(defn ->emacsclient-editor
  "Return the neutral editor capability consumed by a host adapter."
  []
  {:editor/id :emacsclient
   :editor/available? ec/emacs-running?
   :editor/eval
   (fn
     ([code]
      (ec-result->result (ec/eval-elisp code)))
     ([code opts]
      (let [timeout-ms (:timeout-ms opts ec/*default-timeout-ms*)]
        (ec-result->result
         (ec/eval-elisp-with-timeout code timeout-ms)))))
   :editor/feature-available? feature-available?
   :editor/send-to-terminal send-to-terminal})
