(ns hive-emacs.daemon-ranking
  "Daemon capacity, affinity, ranking, and selection."
  (:require [hive-emacs.config :as config]
            [hive-emacs.daemon :as daemon]
            [hive-emacs.daemon-scoring :as scoring]
            [hive-emacs.runtime-ports :as ports]
            [taoensso.timbre :as log]))

(def ^:const max-lings-per-daemon 5)

(defn daemon-ling-count
  "Return the number of lings bound to DAEMON."
  [daemon]
  (count (or (:emacs-daemon/lings daemon) #{})))

(defn daemon-has-capacity?
  "Return true when DAEMON can accept another ling."
  ([daemon]
   (daemon-has-capacity? daemon max-lings-per-daemon))
  ([daemon max-lings]
   (< (daemon-ling-count daemon) max-lings)))

(defn daemon-project-affinity
  "Return the project-affinity score for DAEMON and PROJECT-ID."
  ([daemon project-id]
   (daemon-project-affinity daemon project-id ports/lookup-ling))
  ([daemon project-id lookup-ling-fn]
   (if (nil? project-id)
     0
     (let [ling-ids (or (:emacs-daemon/lings daemon) #{})
           same-project-count
           (->> ling-ids
                (keep lookup-ling-fn)
                (filter #(= project-id (:ling/project-id %)))
                count)]
       (cond
         (>= same-project-count 3) 10
         (pos? same-project-count) 5
         :else 0)))))

(defn score-daemon
  "Return the composite selection score for DAEMON and PROJECT-ID."
  ([daemon project-id]
   (score-daemon daemon project-id ports/lookup-ling))
  ([daemon project-id lookup-ling-fn]
   (let [daemon-id (:emacs-daemon/id daemon)
         status (:emacs-daemon/status daemon)
         health (or (:emacs-daemon/health-score daemon)
                    scoring/default-health-score)
         ling-count (daemon-ling-count daemon)
         not-active (not= :active status)
         at-capacity (not (< ling-count max-lings-per-daemon))
         too-sick (= :unhealthy (scoring/health-level health))
         disqualified? (or not-active at-capacity too-sick)
         capacity-bonus (* (scoring/capacity-left
                            max-lings-per-daemon ling-count)
                           10)
         affinity-bonus (daemon-project-affinity
                         daemon project-id lookup-ling-fn)
         total-score (if disqualified?
                       -1
                       (+ health capacity-bonus affinity-bonus))]
     {:daemon-id daemon-id
      :score total-score
      :ling-count ling-count
      :health-score health
      :health-level (scoring/health-level health)
      :disqualified? disqualified?
      :disqualify-reasons (cond-> []
                            not-active (conj :not-active)
                            at-capacity (conj :at-capacity)
                            too-sick (conj :unhealthy))
      :breakdown {:health health
                  :capacity capacity-bonus
                  :affinity affinity-bonus}})))

(defn select-daemon
  "Return the best daemon for a new ling spawn."
  [store & [{:keys [project-id default-id]}]]
  (let [all-daemons (daemon/get-all-daemons store)
        default-id (or default-id (config/default-daemon-id))]
    (if (empty? all-daemons)
      (do
        (log/warn "No daemons registered, using default:" default-id)
        {:daemon-id default-id
         :reason :no-daemons
         :scored []})
      (let [scored (->> all-daemons
                        (map #(score-daemon % project-id))
                        (sort-by :score #(compare %2 %1)))
            best (first (remove :disqualified? scored))]
        (if best
          (do
            (log/info "Selected daemon:" (:daemon-id best)
                      "score:" (:score best)
                      "lings:" (:ling-count best) "/" max-lings-per-daemon
                      "health:" (:health-level best))
            {:daemon-id (:daemon-id best)
             :reason :selected
             :scored scored})
          (do
            (log/warn "All daemons disqualified! Falling back to default:" default-id
                      "Reasons:"
                      (mapv #(select-keys % [:daemon-id :disqualify-reasons]) scored))
            {:daemon-id default-id
             :reason :all-disqualified
             :scored scored}))))))

(defn update-health-score!
  "Set DAEMON-ID's health score, clamped to 0..100."
  [store daemon-id score]
  {:pre [(string? daemon-id)
         (number? score)]}
  (let [clamped (scoring/clamp-health-score score)]
    (when (daemon/get-daemon store daemon-id)
      (log/debug "Updating daemon health:" daemon-id "→" clamped
                 (scoring/health-level clamped))
      (daemon/update-daemon! store daemon-id
                             {:emacs-daemon/health-score clamped}))))
