(ns hive-emacs.daemon-ds
  "Private DataScript repository for Emacs daemon lifecycle state.

   Daemon entities no longer share a host database. Cross-domain ling data is
   queried through runtime ports, preserving a strict dependency boundary."
  (:require [datascript.core :as d]
            [hive-emacs.daemon :as daemon]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def daemon-schema
  {:emacs-daemon/id {:db/unique :db.unique/identity}
   :emacs-daemon/lings {:db/cardinality :db.cardinality/many}})

(def ^:private stale-threshold-ms (* 2 60 1000))

(defn- daemon-entity->map
  [db eid]
  (when-let [entity (and eid (d/entity db eid))]
    (-> (into {} entity)
        (dissoc :db/id)
        (update :emacs-daemon/lings #(when % (set %))))))

(defn- find-daemon-eid
  [db daemon-id]
  (:db/id (d/entity db [:emacs-daemon/id daemon-id])))

(defn- daemon-attrs
  [attrs]
  (into {}
        (filter (fn [[key _]] (= "emacs-daemon" (namespace key))))
        attrs))

(defrecord DataScriptDaemonStore [connection now-fn]
  daemon/IEmacsDaemon

  (register! [_ daemon-id opts]
    {:pre [(string? daemon-id) (seq daemon-id)]}
    (let [{:keys [socket-name pid emacsclient]} opts
          now (now-fn)
          tx-data (cond-> {:emacs-daemon/id daemon-id
                           :emacs-daemon/socket-name (or socket-name daemon-id)
                           :emacs-daemon/status :active
                           :emacs-daemon/error-count 0
                           :emacs-daemon/started-at now
                           :emacs-daemon/heartbeat-at now}
                    pid (assoc :emacs-daemon/pid pid)
                    emacsclient (assoc :emacs-daemon/emacsclient emacsclient))]
      (log/info "Registering Emacs daemon" {:daemon-id daemon-id})
      (d/transact! connection [tx-data])))

  (heartbeat! [_ daemon-id]
    (let [db @connection]
      (when-let [eid (find-daemon-eid db daemon-id)]
        (d/transact! connection
                     [{:db/id eid
                       :emacs-daemon/heartbeat-at (now-fn)
                       :emacs-daemon/status :active}]))))

  (mark-error! [_ daemon-id error-message]
    (let [db @connection]
      (when-let [eid (find-daemon-eid db daemon-id)]
        (let [previous (or (:emacs-daemon/error-count (d/entity db eid)) 0)]
          (d/transact! connection
                       [{:db/id eid
                         :emacs-daemon/status :error
                         :emacs-daemon/error-message error-message
                         :emacs-daemon/error-count (inc previous)}])))))

  (mark-terminated! [_ daemon-id]
    (let [db @connection]
      (when-let [eid (find-daemon-eid db daemon-id)]
        (d/transact! connection
                     [{:db/id eid :emacs-daemon/status :terminated}]))))

  (update-daemon! [_ daemon-id attrs]
    (let [db @connection]
      (when-let [eid (find-daemon-eid db daemon-id)]
        (d/transact! connection
                     [(assoc (daemon-attrs attrs) :db/id eid)]))))

  (bind-ling! [_ daemon-id ling-id]
    (let [db @connection]
      (when-let [eid (find-daemon-eid db daemon-id)]
        (d/transact! connection
                     [[:db/add eid :emacs-daemon/lings ling-id]]))))

  (unbind-ling! [_ daemon-id ling-id]
    (let [db @connection]
      (when-let [eid (find-daemon-eid db daemon-id)]
        (d/transact! connection
                     [[:db/retract eid :emacs-daemon/lings ling-id]]))))

  (get-daemon [_ daemon-id]
    (let [db @connection]
      (daemon-entity->map db (find-daemon-eid db daemon-id))))

  (get-all-daemons [_]
    (let [db @connection
          eids (d/q '[:find [?e ...]
                      :where [?e :emacs-daemon/id _]]
                    db)]
      (mapv #(daemon-entity->map db %) eids)))

  (get-daemons-by-status [_ status]
    {:pre [(contains? daemon/daemon-statuses status)]}
    (let [db @connection
          eids (d/q '[:find [?e ...]
                      :in $ ?status
                      :where
                      [?e :emacs-daemon/id _]
                      [?e :emacs-daemon/status ?status]]
                    db status)]
      (mapv #(daemon-entity->map db %) eids)))

  (get-daemon-for-ling [_ ling-id]
    (let [db @connection
          eid (d/q '[:find ?e .
                     :in $ ?ling-id
                     :where [?e :emacs-daemon/lings ?ling-id]]
                   db ling-id)]
      (daemon-entity->map db eid)))

  (cleanup-stale! [this]
    (daemon/cleanup-stale! this {}))

  (cleanup-stale! [_ opts]
    (let [threshold (or (:threshold-ms opts) stale-threshold-ms)
          db @connection
          cutoff-ms (- (.getTime ^java.util.Date (now-fn)) threshold)
          active-daemons
          (d/q '[:find ?e ?id ?heartbeat
                 :where
                 [?e :emacs-daemon/id ?id]
                 [?e :emacs-daemon/status :active]
                 [?e :emacs-daemon/heartbeat-at ?heartbeat]]
               db)
          stale-pairs
          (->> active-daemons
               (filter (fn [[_ _ heartbeat]]
                         (< (.getTime ^java.util.Date heartbeat) cutoff-ms)))
               (mapv (fn [[eid id _]] [eid id])))]
      (when (seq stale-pairs)
        (d/transact! connection
                     (mapv (fn [[eid _]]
                             {:db/id eid :emacs-daemon/status :stale})
                           stale-pairs))
        (mapv second stale-pairs)))))

(defn create-store
  "Create an isolated daemon repository.
   Options: :connection DataScript conn, :now-fn (() -> java.util.Date)."
  ([] (create-store {}))
  ([{:keys [connection now-fn]}]
   (->DataScriptDaemonStore
    (or connection (d/create-conn daemon-schema))
    (or now-fn #(java.util.Date.)))))

(defn connection
  "Expose a store's private connection for diagnostics and isolated tests."
  [store]
  (:connection store))
