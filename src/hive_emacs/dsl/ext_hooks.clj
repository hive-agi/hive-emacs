(ns hive-emacs.dsl.ext-hooks
  "`:emacs/*` IAddon hook entries — extension-registry contributions.

   Replaces direct `(:require [hive-mcp.emacs.client :as ec])` imports
   across hive-mcp by registering each callable into the opaque ext
   registry under stable `:emacs/*` keys. Consumers look up via
   `((ext/get-extension :emacs/eval-elisp) ...)` instead of holding a
   compile-time dependency on the hive-emacs namespaces.

   Routed by the IAddon `(hooks [this])` walk in `hive-mcp.addons.core`
   (key namespace ≠ \"multi\" → `hive-mcp.extensions.registry/register!`).
   Lifecycle-bound: deregistered on addon shutdown.

   Decisions:
   - 20260429195812-0c5dfe8d (outphase free-form ext-keys via IAddon hooks)
   - 20260429230453-7e7627cc (extraction + Phase 2 plan)"
  (:require [hive-emacs.client :as ec]
            [hive-emacs.elisp :as el]
            [hive-emacs.notify :as en]
            [hive-emacs.daemon-store :as ds]
            [hive-emacs.olympus :as ol]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def contributions
  "`:emacs/*` keymap merged into `(hooks [this])` alongside `:multi/*`.

   Each value is an IFn — vars deref to functions, so var refs preserve
   dynamic redefinition during REPL development. Surface limited to the
   functions hive-mcp consumers actually call; add new keys here when
   new consumers appear."
  {;; Eval primitives
   :emacs/eval-elisp                  #'ec/eval-elisp
   :emacs/eval-elisp-with-timeout     #'ec/eval-elisp-with-timeout
   :emacs/running?                    #'ec/emacs-running?

   ;; Buffer / file navigation
   :emacs/buffer-list                 #'ec/buffer-list
   :emacs/current-buffer              #'ec/current-buffer
   :emacs/current-file                #'ec/current-file
   :emacs/switch-to-buffer            #'ec/switch-to-buffer
   :emacs/find-file                   #'ec/find-file
   :emacs/save-buffer                 #'ec/save-buffer
   :emacs/goto-line                   #'ec/goto-line
   :emacs/insert-text                 #'ec/insert-text
   :emacs/project-root                #'ec/project-root
   :emacs/recent-files                #'ec/recent-files

   ;; Elisp generation
   :emacs/format-elisp                #'el/format-elisp
   :emacs/require-and-call            #'el/require-and-call
   :emacs/require-and-call-json       #'el/require-and-call-json
   :emacs/require-and-call-text       #'el/require-and-call-text
   :emacs/require-and-call-plist-json #'el/require-and-call-plist-json

   ;; Notification (desktop notify-send)
   :emacs/notify!                     #'en/notify!

   ;; Daemon store (swarm sync)
   :emacs/daemon-store-ensure-default! #'ds/ensure-default-daemon!
   :emacs/daemon-store-select-for-ling #'ds/select-daemon-for-ling
   :emacs/daemon-store-bind-ling!      #'ds/bind-ling!
   :emacs/daemon-store-unbind-ling!    #'ds/unbind-ling!
   :emacs/daemon-store-get-for-ling    #'ds/get-daemon-for-ling
   :emacs/daemon-store-default-id      #'ds/default-daemon-id

   ;; Olympus grid layout (pure)
   :emacs/olympus-calculate-layout    #'ol/calculate-layout
   :emacs/olympus-assign-positions    #'ol/assign-positions
   :emacs/olympus-position-for-cell   #'ol/position-for-cell})
