(ns hive-emacs.cider.port
  "ICiderPort reification over the addon's verb handlers.

   The port is the hive-spi contract face of hive-emacs.tools.cider: same
   params maps in, same MCP response maps out. Registered as the :default
   cider port when the addon contributes its `code` subtree; consumers
   resolve it through hive-spi.cider.registry, never through this ns."
  (:require [hive-emacs.tools.cider :as cider]
            [hive-spi.cider.ports :as ports]
            [hive-spi.cider.registry :as registry]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defrecord EmacsCiderPort []
  ports/ICiderPort
  (cider-eval [_ params] (cider/handle-eval params))
  (cider-doc [_ params] (cider/handle-doc params))
  (cider-info [_ params] (cider/handle-info params))
  (cider-complete [_ params] (cider/handle-complete params))
  (cider-apropos [_ params] (cider/handle-apropos params))
  (cider-status [_ params] (cider/handle-status params))
  (spawn-session [_ params] (cider/handle-spawn params))
  (connect-session [_ params] (cider/handle-connect params))
  (list-sessions [_ params] (cider/handle-sessions params))
  (kill-session [_ params] (cider/handle-kill-session params))
  (kill-all-sessions [_ params] (cider/handle-kill-all params))
  (ensure-connected [_ project-dir] (cider/ensure-connected project-dir)))

(defn ->port
  "An uninitialized EmacsCiderPort. No Emacs or host mutation occurs."
  []
  (->EmacsCiderPort))

(defn register!
  "Install the EmacsCiderPort as the :default cider port. Idempotent."
  []
  (registry/set-port! (->port)))

(defn unregister!
  "Drop the :default cider port. No-op when absent."
  []
  (registry/unregister-port! :default))
