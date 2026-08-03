(ns hive-emacs.elisp
  "Elisp source construction for the Emacs adapter.

   Every builder here now lives in hive-spi.editor.elisp: string in, elisp
   source out, no editor and no lifecycle. This namespace re-exports it so the
   addon's own call sites (and anything that requires hive-emacs.elisp) keep
   working unchanged.

   Usage:
     (require '[hive-emacs.elisp :as el])
     (el/require-and-call-json 'hive-mcp-magit 'hive-mcp-magit-api-status)
     (el/emit '(if (> x 0) \"yes\" \"no\"))"
  (:require [hive-spi.editor.elisp]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(doseq [[sym v] (ns-publics 'hive-spi.editor.elisp)]
  (intern *ns* (with-meta sym (meta v)) (deref v)))
