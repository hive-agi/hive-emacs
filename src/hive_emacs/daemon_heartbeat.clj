(ns hive-emacs.daemon-heartbeat
  "Health-scored heartbeat execution for Emacs daemons."
  (:require [hive-emacs.daemon :as daemon]
            [hive-emacs.daemon-ports :as ports]
            [hive-emacs.daemon-scoring :as scoring]
            [taoensso.timbre :as log]))

(defn heartbeat!
  "Ping DAEMON-ID, persist its health, and return a heartbeat report."
  ([store daemon-id]
   (heartbeat! store daemon-id ports/ping!))
  ([store daemon-id ping-fn]
   (when-let [entity (daemon/get-daemon store daemon-id)]
     (let [prev-score (or (:emacs-daemon/health-score entity)
                            scoring/default-health-score)
             prev-errors (or (:emacs-daemon/error-count entity) 0)
             ling-count (count (or (:emacs-daemon/lings entity) #{}))
             ping-result (try
                           ((or ping-fn ports/ping!) daemon-id)
                           (catch Exception e
                             {:success false :error (.getMessage e)}))
             metrics (scoring/ping-metrics ping-result prev-errors)
             new-score (scoring/compute-health-score
                        prev-score
                        (:latency-ms metrics)
                        (:new-errors metrics)
                        ling-count)
             final-score (scoring/apply-recovery-bonus
                          new-score (:success? metrics) prev-errors)
             report (scoring/heartbeat-report metrics final-score ling-count)
             attrs (cond-> {:emacs-daemon/health-score final-score
                            :emacs-daemon/error-count (:new-errors metrics)
                            :emacs-daemon/status
                            (if (and (not (:success? metrics))
                                     (>= (:new-errors metrics) 3))
                              :error
                              :active)}
                     (:success? metrics)
                     (assoc :emacs-daemon/heartbeat-at (java.util.Date.)))]
       (daemon/update-daemon! store daemon-id attrs)
       (if (:success? metrics)
         (log/debug "Heartbeat OK:" daemon-id
                    "latency:" (:latency-ms metrics) "ms"
                    "health:" final-score (:health-level report)
                    "lings:" ling-count)
         (log/warn "Heartbeat FAILED:" daemon-id
                   "errors:" (:new-errors metrics)
                   "health:" final-score (:health-level report)))
       report))))
