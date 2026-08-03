(ns hive-emacs.editor.services
  "The addon's editor capabilities, published as data into hive-spi.

   Function-level companion to hive-emacs.editor.port: the port answers the
   MCP tool surface, this answers host code that just wants to evaluate
   elisp or read the focused buffer. Both are registered at IAddon mount and
   retracted at shutdown; neither is reachable except through hive-spi, so a
   host never names a hive-emacs namespace.

   Capability keys are the host-facing vocabulary — they intentionally match
   the function names the host used to call directly, so the inversion is a
   change of ROUTE, not of meaning."
  (:require [hive-emacs.client :as ec]
            [hive-emacs.elisp :as el]
            [hive-spi.editor.services :as svc]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def capabilities
  "Capability map published to hive-spi.editor.services.

   Evaluation and editor state come from hive-emacs.client; elisp SOURCE
   construction comes from hive-emacs.elisp — the host generates elisp in
   several places and that generator is the addon's knowledge, not the
   host's."
  {;; evaluation
   :eval-elisp              ec/eval-elisp
   :eval-elisp!             ec/eval-elisp!
   :eval-elisp-with-timeout ec/eval-elisp-with-timeout
   :default-timeout-ms      (fn [] ec/*default-timeout-ms*)

   ;; editor state
   :emacs-running?          ec/emacs-running?
   :buffer-list             ec/buffer-list
   :current-buffer          ec/current-buffer
   :current-file            ec/current-file
   :project-root            ec/project-root
   :recent-files            ec/recent-files

   ;; editor commands
   :switch-to-buffer        ec/switch-to-buffer
   :find-file               ec/find-file
   :save-buffer             ec/save-buffer
   :goto-line               ec/goto-line
   :insert-text             ec/insert-text

   ;; elisp source construction
   :emit                    el/emit
   :emit-forms              el/emit-forms
   :wrap-progn              el/wrap-progn
   :format-elisp            el/format-elisp
   :require-and-call        el/require-and-call
   :require-and-call-json   el/require-and-call-json
   :require-and-call-plist-json el/require-and-call-plist-json
   :require-and-call-text   el/require-and-call-text
   :fboundp-call-json       el/fboundp-call-json})

(defn register!
  "Publish the capability map as the :default editor services. Idempotent."
  []
  (svc/register-services! capabilities))

(defn unregister!
  "Drop the :default editor services. No-op when absent."
  []
  (svc/unregister-services!))
