(ns hive-emacs.editor.port
  "IEditorPort reification over the addon's verb handlers.

   The port is the hive-spi contract face of hive-emacs.tools.emacs: same
   params maps in, same MCP response maps out. Registered as the :default
   editor port when the addon initializes; consumers resolve it through
   hive-spi.editor.registry, never through this ns.

   Surfaces implemented: substrate, buffer, docs. IEditorDaemonPort is
   deliberately absent — hive-emacs' daemon layer has no verb handlers yet, so
   the adapter reports the surface as unsupported rather than reifying methods
   that would throw."
  (:require [hive-emacs.tools.buffer :as buffer]
            [hive-emacs.tools.docs :as docs]
            [hive-emacs.tools.emacs :as emacs]
            [hive-spi.editor.ports :as ports]
            [hive-spi.editor.registry :as registry]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defrecord EmacsEditorPort []
  ports/IEditorPort
  (editor-eval [_ params] (emacs/handle-eval params))
  (editor-notify [_ params] (emacs/handle-notify params))
  (editor-status [_ params] (emacs/handle-status params))
  (editor-capabilities [_ params] (buffer/handle-capabilities params))

  ports/IEditorBufferPort
  (list-buffers [_ params] (emacs/handle-buffers params))
  (current-buffer [_ params] (emacs/handle-current-buffer params))
  (buffer-info [_ params] (buffer/handle-buffer-info params))
  (special-buffers [_ params] (buffer/handle-special-buffers params))
  (switch-buffer [_ params] (emacs/handle-switch-buffer params))
  (find-file [_ params] (emacs/handle-find-file params))
  (save-buffers [_ params] (emacs/handle-save params))
  (goto-line [_ params] (buffer/handle-goto-line params))
  (insert-text [_ params] (buffer/handle-insert-text params))
  (recent-files [_ params] (buffer/handle-recent-files params))
  (project-root [_ params] (buffer/handle-project-root params))
  (editor-context [_ params] (buffer/handle-context params))

  ports/IEditorDocsPort
  (describe-function [_ params] (docs/handle-describe-function params))
  (describe-variable [_ params] (docs/handle-describe-variable params))
  (docs-apropos [_ params] (docs/handle-apropos params))
  (package-functions [_ params] (docs/handle-package-functions params))
  (package-commentary [_ params] (docs/handle-package-commentary params))
  (find-keybindings [_ params] (docs/handle-find-keybindings params))
  (list-packages [_ params] (docs/handle-list-packages params)))

(defn ->port
  "An uninitialized EmacsEditorPort. No Emacs or host mutation occurs."
  []
  (->EmacsEditorPort))

(defn register!
  "Install the EmacsEditorPort as the :default editor port. Idempotent."
  []
  (registry/set-port! (->port)))

(defn unregister!
  "Drop the :default editor port. No-op when absent."
  []
  (registry/unregister-port! :default))
