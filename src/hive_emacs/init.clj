(ns hive-emacs.init
  "IAddon entry point for hive-emacs — Emacs vessel addon.

   Follows nil-railway pipeline (lsp-mcp exemplar):
   resolve-deps → make-addon → register → init → store.

   Provides:
   - IEditor backend via EmacsclientEditor
   - CIDER, Magit, Projectile, buffer, docs tools (collected via requiring-resolve)
   - Daemon lifecycle management

   Elisp loading:
   On initialize!, injects compiled cljel .el files into Emacs load-path
   and loads them (overriding old hive-mcp elisp with migrated cljel versions).
   Follows the hive-claude/lsp-mcp exemplar for load-path injection.

   Usage:
     (init-as-addon!)   ;; called by classpath scanner"
  (:require [hive-emacs.editor-adapter :as ema]
            [hive-emacs.vessel :as vessel]
            [taoensso.timbre :as log]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [hive-emacs.dsl.ext-hooks :as eh]
            [hive-emacs.dsl.multi-hooks :as mh]))
;; Copyright (C) 2024-2026 hive-agi contributors
;;
;; SPDX-License-Identifier: EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0

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

(def ^:private addons-marker
  "Marker file used to locate the elisp output dir on classpath.
   The provide name 'hive-mcp-addons stays stable across builds, so the
   filename hive-mcp-addons.el is a reliable anchor."
  "hive-mcp-addons.el")

(defn- resolve-cljel-dirs
  "Locate compiled cljel elisp directories on classpath.
   Returns vector of absolute directory paths to inject into Emacs load-path."
  []
  (let [dirs (atom [])]
    ;; hive-emacs compiled cljel elisp (flat layout, marker = addons-marker)
    (when-let [res-url (io/resource addons-marker)]
      (let [elisp-dir (-> (.getPath res-url)
                          (str/replace (re-pattern (str "/" addons-marker "$")) ""))]
        (swap! dirs conj elisp-dir)))
    ;; clojure-elisp runtime (required by all compiled .el files)
    (when-let [rt-url (io/resource "clojure-elisp/clojure-elisp-runtime.el")]
      (swap! dirs conj (-> (.getPath rt-url)
                           (str/replace #"/clojure-elisp-runtime\.el$" ""))))
    @dirs))

(defn- collect-cljel-files
  "Collect compiled .el files under the flat elisp/ output dir on classpath.
   Returns absolute paths sorted by name (deterministic load order)."
  []
  (when-let [res-url (io/resource addons-marker)]
    (let [elisp-dir (-> (.getPath res-url)
                        (str/replace (re-pattern (str "/" addons-marker "$")) ""))]
      (->> (file-seq (io/file elisp-dir))
           (filter #(and (.isFile %)
                         (str/ends-with? (.getName %) ".el")))
           (sort-by #(.getName %))
           (mapv #(.getAbsolutePath %))))))

(defonce ^:private elisp-loaded?
  (delay
    (when-let [eval-fn (try-resolve 'hive-mcp.emacs.client/eval-elisp-with-timeout)]
      (let [dirs  (resolve-cljel-dirs)
            files (collect-cljel-files)]
        (when (and (seq dirs) (seq files))
          (log/debug "hive-emacs: cljel load-paths:" dirs)
          ;; Step 1: Inject all dirs into Emacs load-path
          (let [lp-elisp (format "(progn %s t)"
                                 (str/join " " (map #(format "(add-to-list 'load-path \"%s\")" %) dirs)))
                lp-result (eval-fn lp-elisp 5000)]
            (if-not (:success lp-result)
              (do (log/warn "hive-emacs: load-path injection failed:" (:error lp-result))
                  false)
              ;; Step 2: Load each .el file with per-file error handling
              (let [load-forms (mapv (fn [f]
                                       (let [path (str/replace f #"\.el$" "")]
                                         (format "(condition-case err (load \"%s\") (error (message \"hive-emacs cljel: failed %s: %%s\" err)))"
                                                 path (.getName (io/file f)))))
                                     files)
                    load-elisp (format "(progn %s t)" (str/join "\n" load-forms))
                    load-result (eval-fn load-elisp 15000)]
                (if (:success load-result)
                  (do (log/info "hive-emacs: cljel elisp loaded" {:files (count files)})
                      true)
                  (do (log/warn "hive-emacs: cljel load failed:" (:error load-result))
                      false))))))))))

(defn- ensure-elisp-loaded!
  "Ensure compiled cljel elisp is loaded in Emacs. Idempotent."
  []
  @elisp-loaded?)

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
              (let [_elisp-ok? (ensure-elisp-loaded!)
                    set-editor!  (try-resolve 'hive-mcp.protocols.editor/set-editor!)
                    get-editor   (try-resolve 'hive-mcp.protocols.editor/get-editor)
                    editor-id-fn (try-resolve 'hive-mcp.protocols.editor/editor-id)]
                (if (and set-editor! get-editor editor-id-fn)
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
                   :errors ["IEditor protocol not found on classpath"]}))
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
