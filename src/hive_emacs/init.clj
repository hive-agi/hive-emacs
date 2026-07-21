(ns hive-emacs.init
  "Compatibility facade for the historical hive-emacs entrypoint.

   New manifests resolve hive-emacs.addon/addon-ctor directly. These delegates
   remain for older hosts and callers; they construct only. `hive-addon.mount`
   owns registration and lifecycle drive."
  (:require [hive-emacs.addon :as addon]))

;; Copyright (C) 2024-2026 hive-agi contributors
;;
;; SPDX-License-Identifier: MIT OR GPL-2.0-or-later WITH Classpath-exception-2.0

(defn addon-ctor
  [config]
  (addon/addon-ctor config))

(defn init-as-addon!
  ([] (addon/init-as-addon!))
  ([config] (addon/init-as-addon! config)))

(defn ->emacs-addon
  []
  (addon/->emacs-addon))
