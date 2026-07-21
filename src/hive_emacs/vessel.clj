(ns hive-emacs.vessel
  "Host-neutral Emacs vessel capability.

   A host adapter may translate this descriptor into its own vessel protocol.
   Context and capability lookups are injected runtime ports."
  (:require [hive-emacs.runtime-ports :as ports]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defn create-emacs-vessel
  "Return a neutral vessel descriptor with no host implementation dependency."
  []
  {:vessel/id :emacs
   :vessel/capabilities #{:terminal :editor :delivery :repl}
   :vessel/resolve-context
   (fn [agent-id]
     (when agent-id
       (try
         (ports/resolve-agent-context agent-id)
         (catch Exception e
           (log/debug "Emacs vessel context resolution failed"
                      {:agent-id agent-id :error (ex-message e)})
           nil))))
   :vessel/addon
   (fn [capability]
     (when (contains? #{:terminal :editor :delivery :repl} capability)
       (ports/capability capability)))
   :vessel/initialize!
   (fn [config]
     (log/info "Emacs vessel initialized"
               (when config {:config-keys (keys config)}))
     nil)
   :vessel/shutdown!
   (fn []
     (log/info "Emacs vessel shut down")
     nil)})
