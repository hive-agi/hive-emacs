(ns hive-emacs.addon
  "Canonical hive-addon boundary for hive-emacs.

   Construction is pure. Initialization owns only hive-emacs state. Hosts
   adapt declarative tools, hooks, editor, and vessel descriptors through
   their own integration layer."
  (:require [hive-addon.protocol :as addon]
            [hive-emacs.attention :as attention]
            [hive-emacs.bridge-loader :as bridge]
            [hive-emacs.client :as ec]
            [hive-emacs.daemon-store :as daemon-store]
            [hive-emacs.dsl.ext-hooks :as ext-hooks]
            [hive-emacs.dsl.multi-hooks :as multi-hooks]
            [hive-emacs.runtime-ports :as runtime-ports]
            [hive-emacs.tools.cider :as cider-tool]
            [hive-emacs.tools.emacs :as emacs-tool]
            [taoensso.timbre :as log]
            [hive-emacs.editor.port :as editor-port]
            [hive-spi.editor.registry :as registry]
            [hive-emacs.editor.services :as editor-services]
            [hive-emacs.swarm.host :as swarm-host]
            [hive-emacs.vessel :as vessel]
            [hive-spi.vessel :as render-port]
            [hive-vessel.renderer :as renderer]
            [hive-emacs.cider.spawn :as spawn]
            [hive-emacs.crystal.translators :as crystal-translators]
            [hive-emacs.kanban.translators :as kanban-translators]
            [hive-emacs.project.translators :as project-translators]
            [hive-emacs.swarm.translators :as swarm-translators]
            [hive-vessel.core :as vessel-core]))

;; Copyright (C) 2024-2026 hive-agi contributors
;;
;; SPDX-License-Identifier: MIT OR GPL-2.0-or-later WITH Classpath-exception-2.0

(def addon-id-value "hive.emacs")

(def ^:private direct-port-keys
  {:emacs/ping-fn :ping-fn
   :emacs/event-emitter :emit-fn
   :emacs/lookup-ling-fn :lookup-ling-fn
   :emacs/tasks-for-ling-fn :tasks-for-ling-fn
   :emacs/fail-task-fn :fail-task-fn
   :emacs/release-claims-fn :release-claims-fn
   :emacs/update-ling-fn :update-ling-fn
   :emacs/report-daemon-error-fn :report-daemon-error-fn
   :emacs/terminal-dispatch-fn :terminal-dispatch-fn
   :emacs/resolve-agent-context-fn :resolve-agent-context-fn
   :emacs/current-dir-fn :current-dir-fn
   :emacs/capability-fn :capability-fn})

(defn- flatten-config
  [seed runtime-config]
  (let [seed (or seed {})
        runtime-config (or runtime-config {})]
    (merge (:addon/config seed)
           seed
           (:addon/config runtime-config)
           runtime-config)))

(defn- default-ping
  [_daemon-id]
  (ec/eval-elisp-with-timeout "t" 3000))

(defn- default-daemon-error-reporter
  [error-message death-tag]
  (let [daemon-id (daemon-store/default-daemon-id)]
    (daemon-store/mark-error!
     daemon-id
     (str "[" (name death-tag) "] " error-message))))

(defn- port-config
  [config]
  (reduce-kv
   (fn [ports config-key port-key]
     (if (contains? config config-key)
       (assoc ports port-key (get config config-key))
       ports))
   (merge {:ping-fn default-ping
           :report-daemon-error-fn default-daemon-error-reporter}
          (or (:emacs/ports config) {}))
   direct-port-keys))

(defn- eval-fn-of
  "The elisp evaluation boundary this addon starts its Emacs halves through:
   (f code timeout-ms) -> {:success bool ...}. Injectable as `:emacs/eval-fn`,
   so a test cannot reach a live daemon by omission."
  [config]
  (or (:emacs/eval-fn config) ec/eval-elisp-with-timeout))

(defn- ensure-elisp-loaded!
  [eval-fn]
  (try
    (boolean (bridge/ensure-loaded! eval-fn))
    (catch Exception e
      (log/warn "hive-emacs bridge load failed" {:error (ex-message e)})
      false)))

(defn- register-block!
  "Offer EMITTER under KEY through the host's :extension/register! port.
   nil outside a live host; a registration that throws is a warning, never a
   failed initialization."
  [runtime-ports key emitter]
  (when-let [register (:extension/register! runtime-ports)]
    (try
      (register key emitter)
      true
      (catch Exception e
        (log/warn "hive-emacs: block registration failed"
                  {:key key :error (ex-message e)})
        false))))

(defn- register-attention-block!
  "Register the `:block/emacs-attention` emitter. It also keeps the Emacs half
   publishing across Emacs restarts, through EVAL-FN."
  [runtime-ports eval-fn]
  (register-block! runtime-ports
                   attention/extension-key
                   (attention/emitter-with-upkeep eval-fn)))

(defn- register-spawn-block!
  "Register the `:block/cider-spawn` emitter, which reports the outcome of
   each spawned CIDER session once."
  [runtime-ports]
  (register-block! runtime-ports spawn/extension-key spawn/emitter))

(defn- retract-block!
  "The registry has no unregister port; an emitter that says nothing renders
   no block, so that is the retraction."
  [runtime-ports key]
  (when-let [register (:extension/register! runtime-ports)]
    (try
      (register key (constantly nil))
      (catch Exception _ nil))))

(defn- retract-attention-block!
  [runtime-ports]
  (retract-block! runtime-ports attention/extension-key))

(defn- retract-spawn-block!
  [runtime-ports]
  (retract-block! runtime-ports spawn/extension-key))

(defn- initialize-addon!
  [state seed runtime-config]
  (locking state
    (if (= :active (:lifecycle @state))
      {:success? true :already-initialized? true}
      (let [config (flatten-config seed runtime-config)]
        (reset! state {:lifecycle :initializing})
        (try
          (let [ports (port-config config)
                eval-fn (eval-fn-of config)
                _ (runtime-ports/configure! ports)
                _ (daemon-store/ensure-default-daemon!)
                heartbeat-started?
                (boolean
                 (when (:emacs/start-heartbeat? config)
                   (daemon-store/start-heartbeat-loop!)
                   true))
                _ (cider-tool/contribute! (:runtime/ports config))
                _ (emacs-tool/contribute! (:runtime/ports config))
                bridge-ready? (ensure-elisp-loaded! eval-fn)
                attention-block? (boolean
                                  (register-attention-block! (:runtime/ports config) eval-fn))
                attention-publishing? (and bridge-ready?
                                           (attention/enable-in-emacs! eval-fn))
                spawn-block? (boolean
                              (register-spawn-block! (:runtime/ports config)))
                spawn-publishing? (and bridge-ready?
                                       (spawn/enable-in-emacs! eval-fn))
                editor-port (editor-port/register!)
                editor-caps (editor-services/register!)
                swarm-caps (swarm-host/register!)
                metadata {:bridge-ready? bridge-ready?
                          :attention {:block? attention-block?
                                      :publishing? (boolean attention-publishing?)}
                          :cider-spawn {:block? spawn-block?
                                        :publishing? (boolean spawn-publishing?)}
                          :editor-id :emacsclient
                          :editor-surfaces (registry/surfaces editor-port)
                          :editor-capabilities (set (keys editor-caps))
                          :swarm-host-capabilities (set (keys swarm-caps))
                          :heartbeat-started? heartbeat-started?
                          :configured-ports
                          (->> ports
                               (keep (fn [[key value]] (when value key)))
                               set)}]
            (reset! state {:lifecycle :active
                           :metadata metadata
                           :heartbeat-started? heartbeat-started?
                           :runtime/ports (:runtime/ports config)})
            (log/info "hive-emacs initialized" metadata)
            {:success? true :errors [] :metadata metadata})
          (catch Exception e
            (runtime-ports/clear!)
            (let [message (ex-message e)]
              (reset! state {:lifecycle :error :errors [message]})
              (log/error "hive-emacs initialization failed"
                         {:error message})
              {:success? false :errors [message]})))))))

(defn- shutdown-addon!
  [state]
  (locking state
    (cider-tool/retract! (:runtime/ports @state))
    (retract-attention-block! (:runtime/ports @state))
    (retract-spawn-block! (:runtime/ports @state))
    (attention/reset-upkeep!)
    (spawn/reset-watches!)
    (editor-port/unregister!)
    (editor-services/unregister!)
    (swarm-host/unregister!)
    (when (:heartbeat-started? @state)
      (daemon-store/stop-heartbeat-loop!))
    (ec/shutdown-executor!)
    (runtime-ports/clear!)
    (reset! state {:lifecycle :stopped})
    (log/info "hive-emacs shut down"))
  nil)

(defn- addon-health
  [state]
  (let [{:keys [lifecycle metadata errors]} @state]
    (if (= :active lifecycle)
      (try
        (let [running? (boolean (ec/emacs-running?))]
          {:status (if running? :ok :degraded)
           :details (merge metadata {:emacs-running? running?})})
        (catch Exception e
          {:status :degraded
           :details (merge metadata {:error (ex-message e)})}))
      {:status :down
       :details (cond-> {:lifecycle (or lifecycle :new)}
                  (seq errors) (assoc :errors errors))})))

(defrecord HiveEmacsAddon [state seed]
  render-port/IRenderer
  (renderer-id [_] addon-id-value)
  (render! [_ ops] (renderer/deliver! (when (= :active (:lifecycle @state)) (vessel/target)) ops))
  addon/IAddon

  (addon-id [_] addon-id-value)
  (addon-type [_] :native)
  (capabilities [_]
    #{:tools :mcp-bridge :health-reporting :editor :vessel :terminal})

  (initialize! [this runtime-config]
    (let [result (initialize-addon! state seed runtime-config)]
      (when (:success? result) (renderer/register! this))
      result))

  (shutdown! [this]
    (renderer/unregister! this)
    (shutdown-addon! state))

  (tools [_]
    (if (= :active (:lifecycle @state)) emacs-tool/tools []))

  (schema-extensions [_]
    (if (= :active (:lifecycle @state))
      {"code" cider-tool/schema-params}
      []))

  (health [_]
    (addon-health state))

  (excluded-tools [_] #{})

  (hooks [_]
    (if (= :active (:lifecycle @state))
      (merge multi-hooks/contributions
             ext-hooks/contributions
             ;; Swarm / CIDER-session and projectile / project op lowerings
             ;; for any host building a hive-vessel registry from addon hooks.
             {vessel-core/hook-key (concat swarm-translators/translators
                                           project-translators/translators
                                           kanban-translators/translators
                                           crystal-translators/translators)}
             ;; The hive-vessel target: :elisp natives through the bridge-aware
             ;; evaluator. Resolved per call, so it is nil once shut down.
             {vessel/target-hook-key (fn [] (when (= :active (:lifecycle @state))
                                              (vessel/target)))})
      {})))

(defn make-addon
  "Create an uninitialized IAddon. No host or Emacs mutation occurs."
  ([] (make-addon {}))
  ([seed]
   (->HiveEmacsAddon (atom {:lifecycle :new}) (or seed {}))))

(defn addon-ctor
  "Pure hive-addon.mount constructor: config -> uninitialized IAddon."
  [config]
  (make-addon config))

(defn init-as-addon!
  "Compatibility constructor for namespace-scanning mounters."
  ([] (make-addon))
  ([config] (make-addon config)))

(defn ->emacs-addon
  "Compatibility zero-argument constructor."
  []
  (make-addon))
