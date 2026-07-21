(ns hive-emacs.daemon-selection
  "Compatibility facade for daemon ranking, scoring, heartbeat, and redistribution."
  (:require [hive-emacs.daemon-heartbeat :as heartbeat]
            [hive-emacs.daemon-ranking :as ranking]
            [hive-emacs.daemon-redistribution :as redistribution]
            [hive-emacs.daemon-scoring :as scoring]))

(def max-lings-per-daemon ranking/max-lings-per-daemon)
(def default-health-score scoring/default-health-score)

(def health-level scoring/health-level)
(def healthy? scoring/healthy?)
(def daemon-ling-count ranking/daemon-ling-count)
(def daemon-has-capacity? ranking/daemon-has-capacity?)
(def daemon-project-affinity ranking/daemon-project-affinity)
(def score-daemon ranking/score-daemon)
(def select-daemon ranking/select-daemon)
(def update-health-score! ranking/update-health-score!)

(def ewma-alpha scoring/ewma-alpha)
(def latency-healthy-ms scoring/latency-healthy-ms)
(def latency-degraded-ms scoring/latency-degraded-ms)
(def latency-max-penalty scoring/latency-max-penalty)
(def error-penalty scoring/error-penalty)
(def error-penalty-max scoring/error-penalty-max)
(def ling-load-penalty-per scoring/ling-load-penalty-per)
(def recovery-bonus scoring/recovery-bonus)
(def latency-score-delta scoring/latency-score-delta)
(def error-score-delta scoring/error-score-delta)
(def ling-load-delta scoring/ling-load-delta)
(def compute-health-score scoring/compute-health-score)
(def heartbeat! heartbeat/heartbeat!)

(def redistribution-score-threshold
  redistribution/redistribution-score-threshold)
(def max-migrations-per-cycle redistribution/max-migrations-per-cycle)
(def overloaded-ling-threshold redistribution/overloaded-ling-threshold)
(def find-overloaded-daemons redistribution/find-overloaded-daemons)
(def find-migration-candidates redistribution/find-migration-candidates)
(def plan-redistribution redistribution/plan-redistribution)
(def migrate-ling! redistribution/migrate-ling!)
(def redistribute-lings! redistribution/redistribute-lings!)
(def redistribution-status redistribution/redistribution-status)
