(ns hive-emacs.cider.admission
  "RAM admission for owned CIDER JVMs and cleanup of deleted project dirs.

   The addon installs a gate at initialization. `*gate-fn*` is the per-call
   injection seam for tests; production reads MemAvailable through the
   function installed by `configure!`. The reaper takes its session listing,
   stop function, directory probe and shout function as explicit ports."
  (:refer-clojure :exclude [reset!])
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [hive-dsl.result :as result]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def gib (* 1024 1024 1024))
(def default-minimum-gb 12)
(def meminfo-path "/proc/meminfo")
(def jvm-repl-types #{"clj" "cljs" "cljel"})

(defn normalize-dir
  [path]
  (let [value (some-> path str)]
    (when-not (str/blank? value)
      (loop [dir value]
        (if (and (> (count dir) 1) (str/ends-with? dir "/"))
          (recur (subs dir 0 (dec (count dir))))
          dir)))))

(defn read-mem-available
  "MemAvailable in bytes from PATH, or nil when the procfs entry is absent or
   malformed. An absent entry is how non-Linux hosts admit without a gate."
  ([] (read-mem-available meminfo-path))
  ([path]
   (try
     (let [file (io/file path)]
       (when (.isFile file)
         ;; pattern-exempt: the procfs kB field is line-oriented host data;
         ;; a single anchored field reader avoids a pattern-registry dependency.
         (some->> (re-find #"(?m)^MemAvailable:\s+(\d+)\s+kB\s*$" (slurp file))
                  second
                  parse-long
                  (* 1024))))
     (catch Exception e
       (log/warn "cider RAM gate could not read MemAvailable"
                 {:path path :error (ex-message e)})
       nil))))

(defn make-gate
  "An injectable admission function. OPTIONS:
   :mem-available-fn () -> bytes or nil (default: read /proc/meminfo)
   :min-ram-gb        minimum available RAM, default 12
   :log-fn            receives the unsupported-platform message

   The returned fn takes {:caller-id :project-dir :sessions} and answers nil
   to admit or a Result err :cider/insufficient-memory. Nil MemAvailable admits
   and logs once for the lifetime of this gate."
  [{:keys [mem-available-fn min-ram-gb log-fn]
    :or {mem-available-fn read-mem-available
         min-ram-gb default-minimum-gb
         log-fn #(log/info %)}}]
  (when-not (and (number? min-ram-gb) (not (neg? min-ram-gb)))
    (throw (ex-info "cider RAM floor must be a non-negative number of GiB"
                    {:error :cider/invalid-ram-floor :value min-ram-gb})))
  (let [floor-bytes (long (* min-ram-gb gib))
        warned? (atom false)]
    (fn [{:keys [caller-id sessions]}]
      (let [available (try
                        (mem-available-fn)
                        (catch Exception e
                          (log/warn "cider RAM gate port failed; admitting without a reading"
                                    {:error (ex-message e)})
                          nil))]
        (if (nil? available)
          (do
            (when (compare-and-set! warned? false true)
              (log-fn (str "cider RAM admission disabled: /proc/meminfo MemAvailable "
                           "is unavailable; admitting new sessions on this host.")))
            nil)
          (when (< available floor-bytes)
            (let [connected (filter #(= "connected" (:status %)) sessions)
                  owned-by-caller (when-not (str/blank? (str caller-id))
                                    (filter #(or (= caller-id (:owner-id %))
                                                 (= caller-id (:agent-id %)))
                                            connected))
                  reusable (vec (or owned-by-caller []))
                  rows (mapv #(into {}
                                    (remove (comp nil? val))
                                    (cond-> (select-keys % [:name :project-dir :port :agent-id])
                                      (:owner-id %) (assoc :owner-id (:owner-id %))))
                             reusable)
                  names (mapv :name rows)]
              (result/err
               :cider/insufficient-memory
               {:message (str "Error: cider spawn refused: MemAvailable is " available
                              " bytes, below the configured floor of " floor-bytes
                              " bytes (" min-ram-gb " GiB). Reuse or kill one of the "
                              "caller's connected sessions"
                              (if (seq names)
                                (str ": " (str/join ", " names))
                                "; none are currently listed")
                              ". Use `code cider sessions` and `code cider kill-session`.")
                :mem_available_bytes available
                :minimum_ram_bytes floor-bytes
                :minimum_ram_gb min-ram-gb
                :config_key :cider/spawn-min-ram-gb
                :connected_sessions rows}))))))))

(defonce ^:private installed-gate (atom nil))

(def ^:dynamic *gate-fn*
  "Optional per-call RAM gate. Tests bind `(make-gate {...})`; nil uses the
   addon-installed gate, or admits in a bare, uninitialized REPL."
  nil)

(defn configure!
  "Install the addon's RAM gate. See `make-gate` for OPTIONS."
  [options]
  (clojure.core/reset! installed-gate (make-gate options)))

(defn reset!
  "Remove the installed gate at addon shutdown. Returns nil."
  []
  (clojure.core/reset! installed-gate nil)
  nil)

(defn enabled?
  "True when a RAM gate was injected or installed by addon initialization."
  []
  (boolean (or *gate-fn* @installed-gate)))

(defn check
  [context]
  (if-let [gate (or *gate-fn* @installed-gate)]
    (gate context)
    nil))

(defn orphaned?
  "True only for an addon-owned JVM session whose nonblank project-dir fails
   DIR-EXISTS?. External nREPL connections and native REPLs never qualify."
  [session dir-exists?]
  (boolean
   (and (:owned? session)
        (contains? jvm-repl-types (:repl-type session))
        (not (str/blank? (:project-dir session)))
        (not (dir-exists? (:project-dir session))))))

(defn reap!
  "Stop orphaned owned JVM sessions and shout the successfully stopped rows.
   PORTS: :sessions-fn, :stop-fn (returns truthy only after stopping),
   :shout-fn and :directory-exists-fn. One failed stop is reported and does
   not prevent the remaining candidates from being considered."
  [{:keys [sessions-fn stop-fn shout-fn directory-exists-fn]
    :or {directory-exists-fn #(.exists (io/file ^String %))}}]
  (let [outcome (reduce
                 (fn [acc session]
                   (if-not (orphaned? session directory-exists-fn)
                     acc
                     (try
                       (if (stop-fn session)
                         (update acc :stopped conj session)
                         (update acc :skipped conj (:name session)))
                       (catch Exception e
                         (log/warn "cider orphan reaper could not stop a session"
                                   {:name (:name session) :error (ex-message e)})
                         (update acc :failed conj
                                 {:name (:name session) :error (ex-message e)})))))
                 {:stopped [] :skipped [] :failed []}
                 (sessions-fn))
        stopped (:stopped outcome)]
    (when (seq stopped)
      (shout-fn
       (str "cider reaper stopped " (count stopped)
            " owned JVM session(s) whose project-dir no longer exists: "
            (str/join "; "
                      (map #(str (:name %) " (" (:project-dir %) ")") stopped)))))
    outcome))
