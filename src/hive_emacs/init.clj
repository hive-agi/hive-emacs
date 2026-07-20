(ns hive-emacs.init
  "IAddon entry point for hive-emacs — Emacs vessel addon.

   Follows nil-railway pipeline (lsp-mcp exemplar):
   resolve-deps → make-addon → register → init → store.

   Provides:
   - IEditor backend via EmacsclientEditor
   - CIDER, Magit, Projectile, buffer, docs tools (collected via requiring-resolve)
   - Daemon lifecycle management

   Elisp loading:
   On initialize!, delegates to the dependency-free bridge loader, which adds
   compiled Elisp directories to load-path, requires the hive-mcp and CIDER
   entrypoints, validates their callable API, and enables hive-mcp-mode.

   Usage:
     (init-as-addon!)   ;; called by classpath scanner"
  (:require [hive-emacs.bridge-loader :as bridge]
            [hive-emacs.dsl.ext-hooks :as eh]
            [hive-emacs.dsl.multi-hooks :as mh]
            [hive-emacs.editor-adapter :as ema]
            [hive-emacs.vessel :as vessel]
            [taoensso.timbre :as log]))
;; Copyright (C) 2024-2026 hive-agi contributors
;;
;; SPDX-License-Identifier: MIT OR GPL-2.0-or-later WITH Classpath-exception-2.0

(defn- try-resolve
  "Attempt to resolve a fully-qualified symbol. Returns var or nil."
  [sym]
  (try
    (requiring-resolve sym)
    (catch Exception _ nil)))

;; =============================================================================
;; Elisp Load-Path Injection (hive-claude/lsp-mcp exemplar)
;; =============================================================================
;; Compiled cljel .el files live FLAT in elisp/ on the classpath, named by
;; their (provide 'symbol) — emacs idiom (one feature per file, filename
;; matches feature). build.sh outputs there; deps.edn :paths includes it.
;; Source .cljel files stay in src/cljel/ (clean separation like cljs).

(defn- ensure-elisp-loaded!
  "Ensure the Emacs-side hive-mcp and CIDER entrypoints are ready."
  []
  (when-let [eval-fn (try-resolve 'hive-mcp.emacs.client/eval-elisp-with-timeout)]
    (let [loaded? (bridge/ensure-loaded! eval-fn)]
      (if loaded?
        (log/info "hive-emacs: bridge entrypoints ready")
        (log/warn "hive-emacs: bridge entrypoints failed to load"))
      loaded?)))

;; =============================================================================
;; IAddon Implementation
;; =============================================================================

(defonce ^:private addon-instance (atom nil))

(defn- collect-tools
  "Lazily collect tool definitions from Emacs-specific consolidated tool namespace.
   Flat tool namespaces removed — all functionality via consolidated tools."
  []
  (let [tool-nses '[hive-mcp.tools.consolidated.emacs]]
    (->> tool-nses
         (mapcat (fn [ns-sym]
                   (let [tools (try
                                 (require ns-sym)
                                 (let [tools-var (ns-resolve ns-sym 'tools)]
                                   (if tools-var @tools-var []))
                                 (catch Exception e
                                   (log/warn "Failed to load tools from" ns-sym ":" (.getMessage e))
                                   []))]
                     tools)))
         vec)))

(defn- make-addon
  "Create an IAddon reify for hive-emacs.
   Returns nil if protocol is not on classpath."
  []
  (when (try-resolve 'hive-mcp.addons.protocol/IAddon)
    (let [state (atom {:initialized? false})]
      (reify
        hive-mcp.addons.protocol/IAddon

        (addon-id [_] "hive.emacs")

        (addon-type [_] :native)

        (capabilities [_]
          #{:tools :health-reporting :editor})

        (initialize! [_ _config]
          (if (:initialized? @state)
            {:success? true :already-initialized? true}
            (try
              ;; Load compiled cljel elisp into Emacs (idempotent, best-effort)
              (let [elisp-ok? (ensure-elisp-loaded!)
                    set-editor!  (try-resolve 'hive-mcp.protocols.editor/set-editor!)
                    get-editor   (try-resolve 'hive-mcp.protocols.editor/get-editor)
                    editor-id-fn (try-resolve 'hive-mcp.protocols.editor/editor-id)]
                (if (and elisp-ok? set-editor! get-editor editor-id-fn)
                  (do
                    (set-editor! (ema/->emacsclient-editor))
                    ;; Register EmacsVessel in IVessel registry (self-registration)
                    (when-let [v (vessel/create-emacs-vessel)]
                      (when-let [register! (try-resolve 'hive-mcp.protocols.vessel/register-vessel!)]
                        (register! v)
                        (log/info "EmacsVessel registered in vessel registry")))
                    (swap! state assoc :initialized? true)
                    (log/info "hive-emacs initialized — EmacsclientEditor wired as active IEditor")
                    {:success? true
                     :metadata {:editor-id (editor-id-fn (get-editor))}})
                  {:success? false
                   :errors (cond-> []
                             (not elisp-ok?)
                             (conj "Emacs bridge entrypoints failed to load")
                             (not (and set-editor! get-editor editor-id-fn))
                             (conj "IEditor protocol not found on classpath"))}))
              (catch Exception e
                (log/error "hive-emacs initialization failed" {:error (.getMessage e)})
                {:success? false
                 :errors [(.getMessage e)]}))))

        (shutdown! [_]
          (when (:initialized? @state)
            (when-let [unregister! (try-resolve 'hive-mcp.protocols.vessel/unregister-vessel!)]
              (unregister! :emacs))
            (when-let [clear! (try-resolve 'hive-mcp.protocols.editor/clear-editor!)]
              (clear!))
            (swap! state assoc :initialized? false)
            (log/info "hive-emacs shut down — IEditor + EmacsVessel cleared"))
          nil)

        (tools [_]
          (if (:initialized? @state)
            (collect-tools)
            []))

        (schema-extensions [_] {})

        (health [_]
          (if (:initialized? @state)
            (let [get-editor  (try-resolve 'hive-mcp.protocols.editor/get-editor)
                  available?  (try-resolve 'hive-mcp.protocols.editor/available?)
                  editor-id   (try-resolve 'hive-mcp.protocols.editor/editor-id)]
              (if (and get-editor available? editor-id)
                (let [editor    (get-editor)
                      running?  (available? editor)]
                  {:status  (if running? :ok :degraded)
                   :details {:editor-id    (editor-id editor)
                             :emacs-running running?}})
                {:status :degraded
                 :details {:message "IEditor protocol unavailable"}}))
            {:status :down
             :details {:message "hive-emacs not initialized"}}))

        (excluded-tools [_] #{})

        (hooks [_]
          ;; `:multi/*` keys (verbs) → multi.registry; `:emacs/*` keys (callables)
          ;; → opaque ext registry. Routed by namespace dispatch in
          ;; hive-mcp.addons.core init/shutdown walks.
          (merge mh/contributions eh/contributions))))))

;; ---------------------------------------------------------------------------
;; Nil-railway pipeline (lsp-mcp pattern)
;; ---------------------------------------------------------------------------

(defonce ^:private dep-registry
  (atom {:register! 'hive-mcp.addons.core/register-addon!
         :init!     'hive-mcp.addons.core/init-addon!
         :addon-id  'hive-mcp.addons.protocol/addon-id}))

(defn- resolve-deps
  "Resolve all symbols in registry. Returns ctx map or nil."
  [registry]
  (reduce-kv
   (fn [ctx k sym]
     (if-let [resolved (try-resolve sym)]
       (assoc ctx k resolved)
       (do (log/debug "Dep resolution failed:" k "->" sym)
           (reduced nil))))
   {}
   registry))

(defn- step-resolve-deps [ctx]
  (when-let [deps (resolve-deps @dep-registry)]
    (merge ctx deps)))

(defn- step-register [{:keys [addon register!] :as ctx}]
  (let [result (register! addon)]
    (when (:success? result)
      (assoc ctx :reg-result result))))

(defn- step-init [{:keys [addon addon-id init!] :as ctx}]
  (let [result (init! (addon-id addon))]
    (when (:success? result)
      (assoc ctx :init-result result))))

(defn- step-store-instance [{:keys [addon] :as ctx}]
  (reset! addon-instance addon)
  ctx)

(defn- run-addon-pipeline!
  "Nil-railway: resolve-deps → register → init → store"
  [initial-ctx]
  (some-> initial-ctx
          step-resolve-deps
          step-register
          step-init
          step-store-instance))

(defn addon-ctor
  "Pure constructor for the `hive.emacs` IAddon — (config -> IAddon | nil).
   The mounter (hive-addon.mount) resolves this via :addon/init-fn; the host
   then drives register!/initialize!. Returns nil when the IAddon protocol is
   absent from the classpath (graceful, standalone mode). No registration, no
   init!, no side effects — construction only (elisp load + editor/vessel
   wiring run inside the reify's initialize!). Additive: the self-registering
   `init-as-addon!` path remains for the current hive-mcp loader."
  [_config]
  (make-addon))

(defn init-as-addon!
  "Register hive-emacs as an IAddon. Entry point called by classpath scanner."
  []
  (if-let [result (some-> (make-addon)
                          (as-> addon (run-addon-pipeline! {:addon addon})))]
    (do
      (log/info "hive-emacs registered as IAddon")
      {:registered ["hive.emacs"] :total 1})
    (do
      (log/warn "hive-emacs addon registration failed — IAddon unavailable?")
      {:registered [] :total 0})))

(defn get-addon-instance
  "Return the current IAddon instance, or nil."
  []
  @addon-instance)