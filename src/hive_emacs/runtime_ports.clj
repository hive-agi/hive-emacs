(ns hive-emacs.runtime-ports
  "Injected runtime capabilities used by host-neutral hive-emacs services.

   Ports are plain functions installed by IAddon initialization. No symbol in
   this namespace names or resolves a concrete host. Missing query ports return
   empty values; missing mutation ports report an explicit unavailable result."
  (:require [hive-emacs.schema :as schema]
            [malli.core :as m]))

(def ^:private empty-ports
  {:ping-fn nil
   :emit-fn nil
   :lookup-ling-fn nil
   :tasks-for-ling-fn nil
   :fail-task-fn nil
   :release-claims-fn nil
   :update-ling-fn nil
   :report-daemon-error-fn nil
   :terminal-dispatch-fn nil
   :resolve-agent-context-fn nil
   :capability-fn nil})

(defonce ^:private ports (atom empty-ports))

(defn configure!
  "Replace the runtime port set. Unknown keys are rejected."
  [configured]
  (let [configured (or configured {})
        unknown (seq (remove (set (keys empty-ports)) (keys configured)))]
    (when unknown
      (throw (ex-info "Unknown hive-emacs runtime ports"
                      {:unknown (vec unknown)})))
    (reset! ports
            (schema/validate! :hive-emacs/runtime-ports
                              (merge empty-ports configured)))))

(defn clear!
  []
  (reset! ports empty-ports))

(defn snapshot
  []
  @ports)

(defn- unavailable
  [port]
  {:success false
   :success? false
   :error :emacs/port-unavailable
   :port port
   :port-unconfigured true})

(defn ping!
  [daemon-id]
  (if-let [f (:ping-fn @ports)]
    (schema/validate! :hive-emacs/ping-result (f daemon-id))
    (unavailable :ping-fn)))

(defn emit!
  [event payload]
  (when-let [f (:emit-fn @ports)]
    (try
      (f event payload)
      (catch Exception _ nil))))

(defn lookup-ling
  [ling-id]
  (when-let [f (:lookup-ling-fn @ports)]
    (when-let [ling (f ling-id)]
      (schema/validate! :hive-emacs/ling ling))))

(defn tasks-for-ling
  [ling-id status]
  (if-let [f (:tasks-for-ling-fn @ports)]
    (schema/validate! :hive-emacs/tasks
                      (vec (or (f ling-id status) [])))
    []))

(defn fail-task!
  [task-id status]
  (if-let [f (:fail-task-fn @ports)]
    (f task-id status)
    (unavailable :fail-task-fn)))

(defn release-claims!
  [ling-id]
  (if-let [f (:release-claims-fn @ports)]
    (or (f ling-id) 0)
    0))

(defn update-ling!
  [ling-id attrs]
  (if-let [f (:update-ling-fn @ports)]
    (f ling-id attrs)
    (unavailable :update-ling-fn)))

(defn report-daemon-error!
  [error-message death-tag]
  (when-let [f (:report-daemon-error-fn @ports)]
    (f error-message death-tag)))

(defn dispatch-terminal!
  [terminal-id text]
  (if-let [f (:terminal-dispatch-fn @ports)]
    (f terminal-id text)
    (unavailable :terminal-dispatch-fn)))

(defn resolve-agent-context
  [agent-id]
  (when-let [f (:resolve-agent-context-fn @ports)]
    (f agent-id)))

(defn capability
  [capability-key]
  (when-let [f (:capability-fn @ports)]
    (f capability-key)))

(m/=> configure! [:=> [:cat [:maybe :map]] :map])
(m/=> clear! [:=> [:cat] :map])
(m/=> snapshot [:=> [:cat] schema/RuntimePorts])
(m/=> ping! [:=> [:cat :string] schema/PingResult])
(m/=> emit! [:=> [:cat :keyword :map] :any])
(m/=> lookup-ling [:=> [:cat :string] [:maybe schema/Ling]])
(m/=> tasks-for-ling [:=> [:cat :string :keyword] schema/Tasks])
