(ns hive-emacs.no-prompt
  "Agent evaluations never wait on an Emacs prompt.

   Every emacsclient eval is wrapped in `hive-mcp-no-prompt-call`
   (elisp/hive-mcp-no-prompt.el), which turns a minibuffer prompt into an
   error naming it. This namespace builds that wrapper and recognises the
   refusal in emacsclient's stderr. Pure."
  (:require [clojure.string :as str]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def refusal-marker
  "Text every refusal from `hive-mcp-no-prompt-call' carries in its message."
  "refused for agent calls")

(defn refuse-prompts
  "Wrap the elisp form `code` so it runs under `hive-mcp-no-prompt-call`:
   a minibuffer prompt it raises signals an error naming the prompt instead
   of blocking the shared daemon. The helper is required on demand; when it
   cannot be loaded the form runs unwrapped. `code` ends on its own line, so
   a trailing `;` comment cannot swallow the closing parens."
  [code]
  (str "(progn (require 'hive-mcp-no-prompt nil t)"
       " (funcall (if (fboundp 'hive-mcp-no-prompt-call) #'hive-mcp-no-prompt-call #'funcall)"
       " (lambda () " code "\n)))"))

(defn failure-response
  "The response map for an emacsclient run that exited non-zero with stderr
   `err`: {:success false :error <trimmed err>}, plus :prompt-refused true when
   the failure is a refused interactive prompt."
  [err]
  (let [error (str/trim (str err))]
    (cond-> {:success false :error error}
      (str/includes? error refusal-marker) (assoc :prompt-refused true))))
