(ns hive-emacs.dsl.ext-hooks
  "`:emacs/*` IAddon hook entries — declarative capability contributions.

   Consumers adapt stable keys such as `:emacs/eval-elisp` through their own
   extension mechanism instead of importing hive-emacs implementation
   namespaces. The host owns registration and lifecycle binding.

   Decisions:
   - 20260429195812-0c5dfe8d (outphase free-form ext-keys via IAddon hooks)
   - 20260429230453-7e7627cc (extraction + Phase 2 plan)"
  (:require [hive-emacs.bridge-loader :as bridge]
            [hive-emacs.client :as ec]
            [hive-emacs.elisp :as el]
            [hive-emacs.notify :as en]
            [hive-emacs.daemon-store :as ds]
            [hive-emacs.editor-adapter :as editor]
            [hive-emacs.olympus :as ol]
            [hive-emacs.vessel :as vessel]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defn eval-elisp-with-timeout
  "Ensure bridge entrypoints, then evaluate CODE with TIMEOUT-MS."
  [code timeout-ms]
  (bridge/eval-with-bridge ec/eval-elisp-with-timeout code timeout-ms))

(defn eval-elisp
  "Ensure bridge entrypoints, then evaluate CODE with the default timeout."
  [code]
  (eval-elisp-with-timeout code ec/*default-timeout-ms*))

(def contributions
  "`:emacs/*` keymap merged into `(hooks [this])` alongside `:multi/*`.

   Each value is an IFn — vars deref to functions, so var refs preserve
   dynamic redefinition during REPL development. Surface limited to the
   intentionally narrow; add keys only for supported public capabilities."
  {;; Eval primitives
   :emacs/eval-elisp                  #'eval-elisp
   :emacs/eval-elisp-with-timeout     #'eval-elisp-with-timeout
   :emacs/running?                    #'ec/emacs-running?
   :emacs/editor                      #'editor/->emacsclient-editor
   :emacs/vessel                      #'vessel/create-emacs-vessel

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
