(ns hive-emacs.vessel
  "EmacsVessel — IVessel implementation for the Emacs host environment.

   Uses requiring-resolve for all hive-mcp dependencies (same pattern as
   hive-emacs/init.clj). Slave lookup goes through ISwarmRegistry/get-slave
   protocol method — backend-agnostic (DataScript, Datalevin, etc.).

   Delegates to existing protocols for capabilities:
   - resolve-context: ISwarmRegistry/get-slave (protocol, not direct queries)
   - :terminal: terminal-registry (ITerminalAddon)
   - :editor: IEditor (protocols/editor)
   - :delivery: IDeliveryChannel (websocket channel)
   - :repl: nil (future: ICiderSession)"
  (:require [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ============================================================================
;;; Dependency Resolution (requiring-resolve, no direct coupling)
;;; ============================================================================

(defn- try-resolve
  "Attempt to resolve a fully-qualified symbol. Returns var or nil."
  [sym]
  (try
    (requiring-resolve sym)
    (catch Exception _ nil)))

(defn- get-slave-via-protocol
  "Look up a slave through ISwarmRegistry/get-slave (backend-agnostic).
   Returns slave map or nil."
  [agent-id]
  (when-let [get-registry (try-resolve 'hive-mcp.swarm.datascript.registry/get-default-registry)]
    (when-let [get-slave (try-resolve 'hive-mcp.swarm.protocol/get-slave*)]
      (get-slave (get-registry) agent-id))))

(defn- derive-project-id
  "Derive project-id from directory via scope lookup. Returns string or nil."
  [directory]
  (when directory
    (when-let [get-pid (try-resolve 'hive-mcp.tools.memory.scope/get-current-project-id)]
      (get-pid directory))))

(defn- resolve-terminal-addon
  "Get primary terminal addon via terminal registry."
  []
  (when-let [get-fn (try-resolve 'hive-mcp.agent.ling.terminal-registry/get-terminal-addon)]
    (get-fn :vterm)))

(defn- resolve-editor
  "Get active IEditor (returns nil if noop)."
  []
  (when-let [get-fn (try-resolve 'hive-mcp.protocols.editor/get-editor)]
    (let [editor (get-fn)]
      (when-let [id-fn (try-resolve 'hive-mcp.protocols.editor/editor-id)]
        (when (not= :noop (id-fn editor))
          editor)))))

(defn- resolve-delivery-channel
  "Get websocket delivery channel."
  []
  (when-let [get-fn (try-resolve 'hive-mcp.protocols.delivery-channel/get-channel)]
    (get-fn :websocket)))

;;; ============================================================================
;;; EmacsVessel
;;; ============================================================================

(defn create-emacs-vessel
  "Create an EmacsVessel implementing IVessel.
   Returns nil if IVessel protocol is not on classpath."
  []
  (when-let [_proto (try-resolve 'hive-mcp.protocols.vessel/IVessel)]
    (reify
      hive-mcp.protocols.vessel/IVessel

      (vessel-id [_] :emacs)

      (capabilities [_] #{:terminal :editor :delivery :repl})

      (resolve-context [_ agent-id]
        ;; Use ISwarmRegistry/get-slave (protocol method, backend-agnostic).
        ;; Slave stores :slave/cwd and :slave/project-id at spawn time.
        ;; Returns nil for unknown agents — fallback chain continues.
        (when agent-id
          (try
            (when-let [slave (get-slave-via-protocol agent-id)]
              (let [cwd (:slave/cwd slave)
                    project-id (or (:slave/project-id slave)
                                   (derive-project-id cwd))]
                (when (or cwd project-id)
                  {:cwd cwd
                   :project-id project-id})))
            (catch Exception e
              (log/debug "EmacsVessel resolve-context failed for" agent-id ":" (ex-message e))
              nil))))

      (addon [_ capability]
        (case capability
          :terminal (resolve-terminal-addon)
          :editor   (resolve-editor)
          :delivery (resolve-delivery-channel)
          :repl     nil
          nil))

      (initialize! [_ config]
        (log/info "EmacsVessel initialized" (when config {:config-keys (keys config)}))
        nil)

      (shutdown! [_]
        (log/info "EmacsVessel shut down")
        nil))))
