(ns hive-emacs.addon
  "Canonical hive-addon boundary for hive-emacs.

   Construction is pure. Initialization owns only hive-emacs state. Hosts
   adapt declarative tools, hooks, editor, and vessel descriptors through
   their own integration layer."
  (:require [hive-addon.protocol :as addon]
            [hive-emacs.bridge-loader :as bridge]
            [hive-emacs.client :as ec]
            [hive-emacs.daemon-store :as daemon-store]
            [hive-emacs.dsl.ext-hooks :as ext-hooks]
            [hive-emacs.dsl.multi-hooks :as multi-hooks]
            [hive-emacs.runtime-ports :as runtime-ports]
            [hive-emacs.tools.emacs :as emacs-tool]
            [taoensso.timbre :as log]))

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

(defn- ensure-elisp-loaded!
  []
  (try
    (boolean (bridge/ensure-loaded! ec/eval-elisp-with-timeout))
    (catch Exception e
      (log/warn "hive-emacs bridge load failed" {:error (ex-message e)})
      false)))

(defn- initialize-addon!
  [state seed runtime-config]
  (locking state
    (if (= :active (:lifecycle @state))
      {:success? true :already-initialized? true}
      (let [config (flatten-config seed runtime-config)]
        (reset! state {:lifecycle :initializing})
        (try
          (let [ports (port-config config)
                _ (runtime-ports/configure! ports)
                _ (daemon-store/ensure-default-daemon!)
                heartbeat-started?
                (boolean
                 (when (:emacs/start-heartbeat? config)
                   (daemon-store/start-heartbeat-loop!)
                   true))
                bridge-ready? (ensure-elisp-loaded!)
                metadata {:bridge-ready? bridge-ready?
                          :editor-id :emacsclient
                          :heartbeat-started? heartbeat-started?
                          :configured-ports
                          (->> ports
                               (keep (fn [[key value]] (when value key)))
                               set)}]
            (reset! state {:lifecycle :active
                           :metadata metadata
                           :heartbeat-started? heartbeat-started?})
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
  addon/IAddon

  (addon-id [_] addon-id-value)
  (addon-type [_] :native)
  (capabilities [_]
    #{:tools :mcp-bridge :health-reporting :editor :vessel :terminal})

  (initialize! [_ runtime-config]
    (initialize-addon! state seed runtime-config))

  (shutdown! [_]
    (shutdown-addon! state))

  (tools [_]
    (if (= :active (:lifecycle @state)) emacs-tool/tools []))

  (schema-extensions [_] [])

  (health [_]
    (addon-health state))

  (excluded-tools [_] #{})

  (hooks [_]
    (if (= :active (:lifecycle @state))
      (merge multi-hooks/contributions ext-hooks/contributions)
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
