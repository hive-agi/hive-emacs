(ns hive-emacs.test-support
  "Host-free test fixtures and an in-memory implementation of runtime ports."
  (:require [datascript.core :as d]
            [hive-emacs.client :as client]
            [hive-emacs.daemon-ds :as daemon-ds]
            [hive-emacs.daemon-store :as daemon-store]
            [hive-emacs.runtime-ports :as ports]
            [hive-test.isolation :as isolation]))

(defn empty-world
  []
  {:lings {}
   :tasks {}
   :claims {}
   :events []})

(defn reset-store!
  [store]
  (d/reset-conn! (daemon-ds/connection store)
                 (d/empty-db daemon-ds/daemon-schema))
  store)

(defn reset-world!
  [world]
  (reset! world (empty-world)))

(defn add-ling!
  ([world ling-id status]
   (add-ling! world ling-id status nil))
  ([world ling-id status project-id]
   (swap! world assoc-in [:lings ling-id]
          (cond-> {:ling/id ling-id :ling/status status}
            project-id (assoc :ling/project-id project-id)))
   (get-in @world [:lings ling-id])))

(defn add-task!
  [world task-id ling-id status]
  (swap! world assoc-in [:tasks task-id]
         {:task/id task-id :task/ling-id ling-id :task/status status})
  (get-in @world [:tasks task-id]))

(defn add-claim!
  [world file-path ling-id]
  (swap! world assoc-in [:claims file-path]
         {:claim/file file-path :claim/ling-id ling-id}))

(defn install-world!
  "Install pure function ports backed by WORLD. Optional overrides win."
  ([world] (install-world! world {}))
  ([world overrides]
   (ports/configure!
    (merge
     {:ping-fn (fn [_]
                 {:success true :duration-ms 1})
      :lookup-ling-fn #(get-in @world [:lings %])
      :tasks-for-ling-fn
      (fn [ling-id status]
        (->> (:tasks @world)
             vals
             (filterv #(and (= ling-id (:task/ling-id %))
                            (= status (:task/status %))))))
      :fail-task-fn
      (fn [task-id status]
        (swap! world assoc-in [:tasks task-id :task/status] status)
        (get-in @world [:tasks task-id]))
      :release-claims-fn
      (fn [ling-id]
        (let [owned (->> (:claims @world)
                         (keep (fn [[path claim]]
                                 (when (= ling-id (:claim/ling-id claim)) path)))
                         vec)]
          (swap! world update :claims #(apply dissoc % owned))
          (count owned)))
      :update-ling-fn
      (fn [ling-id attrs]
        (swap! world update-in [:lings ling-id] merge attrs)
        (get-in @world [:lings ling-id]))
      :emit-fn
      (fn [event payload]
        (swap! world update :events conj [event payload]))}
     overrides))))

(defmethod isolation/emit-isolation :hive-emacs/runtime
  [{:keys [store world ports]}]
  (fn [test-fn]
    (daemon-store/reset-store!)
    (when store (reset-store! store))
    (when world
      (reset-world! world)
      (install-world! world ports))
    (try
      (test-fn)
      (finally
        (daemon-store/stop-heartbeat-loop!)
        (client/shutdown-executor!)
        (ports/clear!)))))
