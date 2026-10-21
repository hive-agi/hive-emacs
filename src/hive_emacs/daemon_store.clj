(ns hive-emacs.daemon-store
  "Singleton daemon store for shared access across modules.

   Provides a shared IEmacsDaemon store instance that can be accessed
   from swarm sync, emacsclient, and coordinator modules.

   DDD: Infrastructure layer - singleton access to repository."
  (:require [hive-emacs.config :as config]
            [hive-emacs.daemon :as daemon]
            [hive-emacs.daemon-liveness :as liveness]
            [hive-emacs.daemon-ds :as daemon-ds]
            [hive-emacs.daemon-autoheal :as autoheal]
            [hive-emacs.daemon-ranking :as ranking]
            [hive-emacs.daemon-redistribution :as redistribution]
            [hive-emacs.daemon-scoring :as scoring]
            [taoensso.timbre :as log]
            [hive-emacs.daemon-ports :as ports]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT


(defonce ^{:private true
           :doc "Lazily initialized, replaceable daemon repository."}
  daemon-store-state
  (atom nil))

(defn get-store
  "Get the shared daemon store instance.
   Lazily initializes on first call.

   Returns:
     IEmacsDaemon implementation (DataScriptDaemonStore)"
  []
  (or @daemon-store-state
      (let [candidate (daemon-ds/create-store)]
        (if (compare-and-set! daemon-store-state nil candidate)
          (do
            (log/info "Initialized singleton daemon store")
            candidate)
          @daemon-store-state))))

(defn reset-store!
  "Replace the singleton repository. Intended for lifecycle reset and tests."
  ([] (reset-store! (daemon-ds/create-store)))
  ([store]
   (reset! daemon-store-state store)
   store))


(defn register!
  "Register a new Emacs daemon. Delegates to IEmacsDaemon.register!"
  [daemon-id opts]
  (daemon/register! (get-store) daemon-id opts))

(defn heartbeat!
  "Update daemon heartbeat. Delegates to IEmacsDaemon.heartbeat!"
  [daemon-id]
  (daemon/heartbeat! (get-store) daemon-id))

(defn mark-error!
  "Mark daemon as errored. Delegates to IEmacsDaemon.mark-error!"
  [daemon-id error-message]
  (daemon/mark-error! (get-store) daemon-id error-message))

(defn mark-terminated!
  "Mark daemon as terminated. Delegates to IEmacsDaemon.mark-terminated!"
  [daemon-id]
  (daemon/mark-terminated! (get-store) daemon-id))

(defn bind-ling!
  "Bind a ling to a daemon. Delegates to IEmacsDaemon.bind-ling!"
  [daemon-id ling-id]
  (daemon/bind-ling! (get-store) daemon-id ling-id))

(defn unbind-ling!
  "Unbind a ling from a daemon. Delegates to IEmacsDaemon.unbind-ling!"
  [daemon-id ling-id]
  (daemon/unbind-ling! (get-store) daemon-id ling-id))

(defn get-daemon
  "Get daemon by ID. Delegates to IEmacsDaemon.get-daemon"
  [daemon-id]
  (daemon/get-daemon (get-store) daemon-id))

(defn get-all-daemons
  "Get all registered daemons. Delegates to IEmacsDaemon.get-all-daemons"
  []
  (daemon/get-all-daemons (get-store)))

(defn get-daemons-by-status
  "Get daemons by status. Delegates to IEmacsDaemon.get-daemons-by-status"
  [status]
  (daemon/get-daemons-by-status (get-store) status))

(defn get-daemon-for-ling
  "Find daemon for a ling. Delegates to IEmacsDaemon.get-daemon-for-ling"
  [ling-id]
  (daemon/get-daemon-for-ling (get-store) ling-id))

(defn cleanup-stale!
  "Cleanup stale daemons. Delegates to IEmacsDaemon.cleanup-stale!"
  ([]
   (daemon/cleanup-stale! (get-store)))
  ([opts]
   (daemon/cleanup-stale! (get-store) opts)))


(defn default-daemon-id
  "Get the default daemon ID from environment or use 'server'.
   This matches the socket name used by *emacs-socket-name* in emacsclient.clj.
   Delegates to hive-emacs.config — single source of truth."
  []
  (config/default-daemon-id))

(defn ensure-default-daemon!
  "Ensure the default daemon is registered.
   Call this during system startup to bootstrap daemon tracking.

   Returns:
     Transaction report from register! or nil if already exists"
  []
  (let [id (default-daemon-id)]
    (when-not (get-daemon id)
      (log/info "Registering default daemon:" id)
      (register! id {:socket-name id}))))


(defn select-daemon-for-ling
  "Select the best daemon for a new ling spawn.

   Uses health scoring, capacity limits, and project affinity to pick
   the optimal daemon. Falls back to default daemon if no better option.

   Arguments:
     opts - Optional map with:
            :project-id - Target project for affinity scoring

   Returns:
     Map with :daemon-id and :reason (:selected, :no-daemons, :all-disqualified)"
  ([]
   (select-daemon-for-ling {}))
  ([opts]
   (ranking/select-daemon (get-store)
                          (merge opts {:default-id (default-daemon-id)}))))

(defn update-daemon-health!
  "Update a daemon's health score. Delegates to daemon-selection.

   Arguments:
     daemon-id - Daemon to update
     score     - New health score (0-100)"
  [daemon-id score]
  (ranking/update-health-score! (get-store) daemon-id score))


(def ^:private heartbeat-interval-ms
  "Interval between heartbeats in milliseconds. Default: 30 seconds."
  30000)

(def ^:private stale-cleanup-interval-ms
  "Interval between stale daemon cleanup checks. Default: 2 minutes."
  (* 2 60 1000))

(defonce ^{:private true
           :doc "Heartbeat loop state: {:running? bool :thread Thread :last-heartbeat inst :last-cleanup inst}"}
  heartbeat-state
  (atom {:running? false
         :thread nil
         :last-heartbeat nil
         :last-cleanup nil}))

(defn- heartbeat-opts
  "Production ports for one liveness-first heartbeat."
  []
  {:ping-fn ports/ping!
   :liveness-fn liveness/pid-liveness
   :now-fn #(java.util.Date.)})

(defn- periodic-maintenance!
  "Stale cleanup, W3 auto-heal and W4 redistribution over STORE. Store-only:
   none of these spawns emacsclient."
  [store]
  (when-let [stale-ids (seq (daemon/cleanup-stale! store))]
    (log/warn "Heartbeat: marked" (count stale-ids) "daemons as stale:" stale-ids))
  (when-let [heal-result (autoheal/heal-all-orphans! store)]
    (log/warn "Heartbeat: auto-healed" (:healed heal-result)
              "of" (:orphans-found heal-result) "orphaned lings"
              (when (pos? (:failed heal-result))
                (str "(" (:failed heal-result) " failed)")))
    (swap! heartbeat-state assoc
           :last-autoheal (java.util.Date.)
           :last-autoheal-result (select-keys heal-result
                                              [:orphans-found :healed :failed])))
  (when-let [redist-result (redistribution/redistribute-lings! store)]
    (log/info "Heartbeat: redistributed" (:migrations-executed redist-result)
              "of" (:migrations-planned redist-result) "planned migrations"
              (when (pos? (:migrations-failed redist-result))
                (str "(" (:migrations-failed redist-result) " failed)")))
    (swap! heartbeat-state assoc
           :last-redistribution (java.util.Date.)
           :last-redistribution-result (select-keys redist-result
                                                    [:migrations-planned
                                                     :migrations-executed
                                                     :migrations-failed]))))

(defn heartbeat-tick!
  "One heartbeat cycle over STORE at NOW-MS: a liveness-first heartbeat for
   every active daemon (emacsclient only for a daemon whose health dropped,
   see `hive-emacs.daemon-liveness`), then the periodic maintenance when
   `stale-cleanup-interval-ms` has passed since LAST-CLEANUP-MS. OPTS are the
   liveness ports {:ping-fn :liveness-fn :now-fn}. Returns the new
   last-cleanup-ms."
  [store opts now-ms last-cleanup-ms]
  (let [reports (liveness/heartbeat-active! store (default-daemon-id) opts)]
    (when-let [r (last reports)]
      (swap! heartbeat-state assoc
             :last-heartbeat (java.util.Date.)
             :last-health-score (:health-score r)
             :last-health-level (:health-level r)
             :last-pings (count (filter #(= :ping (:probe %)) reports))
             :last-probes (count reports))))
  (if (>= (- now-ms last-cleanup-ms) stale-cleanup-interval-ms)
    (do (periodic-maintenance! store)
        (swap! heartbeat-state assoc :last-cleanup (java.util.Date.))
        now-ms)
    last-cleanup-ms))

(defn- heartbeat-loop
  "Background loop: one `heartbeat-tick!` every `heartbeat-interval-ms`
   until running? becomes false."
  []
  (log/info "Daemon heartbeat loop started (liveness-first)")
  (let [last-cleanup-at (atom (System/currentTimeMillis))]
    (while (:running? @heartbeat-state)
      (try
        (reset! last-cleanup-at
                (heartbeat-tick! (get-store)
                                 (heartbeat-opts)
                                 (System/currentTimeMillis)
                                 @last-cleanup-at))
        (catch Exception e
          (log/error "Heartbeat loop error:" (.getMessage e))))
      ;; Interrupt is the normal shutdown signal, not an uncaught daemon error.
      (when (:running? @heartbeat-state)
        (try
          (Thread/sleep heartbeat-interval-ms)
          (catch InterruptedException _
            nil)))))
  (log/info "Daemon heartbeat loop stopped"))

(defn start-heartbeat-loop!
  "Start the background heartbeat loop.
   Idempotent - does nothing if already running.

   Returns:
     Current heartbeat state"
  []
  (if (:running? @heartbeat-state)
    (do
      (log/debug "Heartbeat loop already running")
      @heartbeat-state)
    (let [thread (Thread. ^Runnable heartbeat-loop "daemon-heartbeat")]
      (.setDaemon thread true) ; JVM can exit without waiting for this thread
      (swap! heartbeat-state assoc :running? true :thread thread)
      (.start thread)
      @heartbeat-state)))

(defn stop-heartbeat-loop!
  "Stop the background heartbeat loop.
   Idempotent - does nothing if not running.

   Returns:
     Current heartbeat state"
  []
  (if-not (:running? @heartbeat-state)
    (do
      (log/debug "Heartbeat loop not running")
      @heartbeat-state)
    (do
      (swap! heartbeat-state assoc :running? false)
      ;; Thread will exit on next iteration
      (when-let [thread (:thread @heartbeat-state)]
        (.interrupt thread))
      (swap! heartbeat-state assoc :thread nil)
      @heartbeat-state)))

(defn heartbeat-status
  "Get current heartbeat loop status.

   Returns:
     Map with :running?, :last-heartbeat, :last-cleanup, :daemon-status,
     :health-score, :health-level (W2 additions),
     :last-autoheal, :last-autoheal-result (W3 additions),
     :last-redistribution, :last-redistribution-result (W4 additions),
     :last-pings, :last-probes (emacsclient pings vs daemons probed in the
     last cycle)"
  []
  (let [daemon-id (default-daemon-id)
        daemon (get-daemon daemon-id)]
    (merge (select-keys @heartbeat-state [:running? :last-heartbeat :last-cleanup
                                          :last-health-score :last-health-level
                                          :last-pings :last-probes
                                          :last-autoheal :last-autoheal-result
                                          :last-redistribution :last-redistribution-result])
           {:daemon-id daemon-id
            :daemon-status (:emacs-daemon/status daemon)
            :daemon-error-count (:emacs-daemon/error-count daemon)
            :daemon-health-score (:emacs-daemon/health-score daemon)
            :daemon-health-level (when-let [s (:emacs-daemon/health-score daemon)]
                                   (scoring/health-level s))})))


(defn heal-orphans!
  "Manually trigger orphan detection and healing.
   Useful for immediate cleanup outside the heartbeat cycle.

   Returns:
     Map with :orphans-found, :healed, :failed, :results
     or nil if no orphans found."
  []
  (autoheal/heal-all-orphans! (get-store)))

(defn orphan-status
  "Get current orphan status without healing. Monitoring endpoint.

   Returns:
     Map with :orphan-count, :orphans (with classified actions)
     or nil if no orphans."
  []
  (autoheal/orphan-status (get-store)))


(defn redistribute-lings!
  "Manually trigger ling redistribution from overloaded/degraded daemons.
   Useful for immediate rebalancing outside the heartbeat cycle.

   Returns:
     Map with :migrations-planned, :migrations-executed, :migrations-failed, :results
     or nil if no redistribution needed."
  []
  (redistribution/redistribute-lings! (get-store)))

(defn redistribution-status
  "Get current redistribution status without executing. Monitoring endpoint.

   Returns:
     Map with :overloaded-count, :overloaded-daemons, :planned-migrations
     or nil if system is balanced."
  []
  (redistribution/redistribution-status (get-store)))
