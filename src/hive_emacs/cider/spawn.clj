(ns hive-emacs.cider.spawn
  "A spawn answers \"starting\"; the outcome lands seconds later. This namespace
   remembers the spawns an agent asked for and reports each outcome once, as
   the ---CIDER-SPAWN--- block on the next tool response.

   The verdict is read from `<root>/<server>/cider-spawns.json`, published by
   the Emacs half, never over emacsclient: a jack-in prompt blocks emacsclient
   exactly while a spawn is pending.

   Layers: value objects live in hive-emacs.schema; parse/verdict/describe are
   pure; the ledger, the directory read and the emitter are the shell."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [hive-dsl.result :as result]
            [hive-emacs.attention :as attention]
            [hive-emacs.schema :as schema]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def extension-key
  "Host block-emitter key. The key NAME is the wire tag: ---CIDER-SPAWN---."
  :block/cider-spawn)

(def state-file-name "cider-spawns.json")

(def failed-statuses
  "Registry statuses that mean the session will not serve an eval."
  #{"error" "timeout" "stale"})

(def connected-status "connected")

(def base-deadline-ms
  "Budget for a clj spawn: the connect retry window plus the nREPL handshake."
  90000)

(def upgrade-deadline-ms
  "Extra budget for a REPL that must be upgraded after connecting."
  45000)

(def upgraded-repl-types #{"cljs" "cljel"})

;; ── pure core ───────────────────────────────────────────────────────────────

(defn deadline-ms
  "Milliseconds a spawn of REPL-TYPE may take before its silence is a verdict."
  [repl-type]
  (if (contains? upgraded-repl-types repl-type)
    (+ base-deadline-ms upgrade-deadline-ms)
    base-deadline-ms))

(defn parse-state
  "One published session row -> CiderSpawnState, or nil when off-contract."
  [server heartbeat-ms raw]
  (let [state (cond-> {:name (:name raw)
                       :port (when (integer? (:port raw)) (long (:port raw)))
                       :repl-type (or (:repl-type raw) "clj")
                       :status (or (:status raw) "unknown")
                       :server server
                       :heartbeat-ms heartbeat-ms}
                (string? (:reason raw)) (assoc :reason (:reason raw))
                (string? (:agent-id raw)) (assoc :agent-id (:agent-id raw))
                (string? (:project-dir raw)) (assoc :project-dir (:project-dir raw)))]
    (when (schema/valid? :hive-emacs/cider-spawn-state state)
      state)))

(defn parse-registry
  "JSON text of cider-spawns.json -> session states. `daemon-dir` is the
   directory name, used when the file omits :server. Never throws."
  [daemon-dir text]
  (try
    (let [raw (json/read-str text :key-fn keyword)
          server (or (:server raw) daemon-dir)
          heartbeat-ms (some-> (:heartbeat raw) (* 1000) long)]
      (into [] (keep #(parse-state server heartbeat-ms %)) (:sessions raw)))
    (catch Exception _ [])))

(defn ->watch
  "The payload a spawn answered -> CiderSpawnWatch, or nil when it is not a
   spawn acknowledgement."
  [payload now-ms]
  (let [raw (if (string? payload)
              (result/rescue nil (json/read-str payload :key-fn keyword))
              payload)]
    (when (map? raw)
      (let [nm (:name raw)
            repl-type (or (:repl-type raw) (:repl_type raw) "clj")
            watch {:name (when (string? nm) nm)
                   :port (when (integer? (:port raw)) (long (:port raw)))
                   :repl-type repl-type
                   :project-dir (when (string? (:project-dir raw)) (:project-dir raw))
                   :requested-ms now-ms
                   :deadline-ms (deadline-ms repl-type)}]
        (when (schema/valid? :hive-emacs/cider-spawn-watch watch)
          watch)))))

(defn elapsed-ms
  [watch now-ms]
  (max 0 (- now-ms (:requested-ms watch))))

(defn overdue?
  [watch now-ms]
  (>= (elapsed-ms watch now-ms) (:deadline-ms watch)))

(defn verdict
  "Where WATCH stands given the STATE the Emacs half published for it, nil when
   it published none. PUBLISHED? says whether the Emacs half published a
   registry AT ALL: silence from a publisher that is not running is not
   evidence about the session, so it is its own verdict."
  ([watch state now-ms] (verdict watch state now-ms true))
  ([watch state now-ms published?]
   (let [status (:status state)]
     (cond
       (= connected-status status) :connected
       (contains? failed-statuses status) :failed
       (not (overdue? watch now-ms)) :pending
       (some? state) :overdue
       published? :vanished
       :else :unpublished))))

(defn settled?
  "True for a verdict that is the outcome — reported once, then forgotten."
  [verdict]
  (not= :pending verdict))

(defn- endpoint
  [watch state]
  (let [port (or (:port state) (:port watch))
        repl-type (or (:repl-type state) (:repl-type watch))]
    (str repl-type (when port (str ", port " port)))))

(defn- waited
  [watch now-ms]
  (str (quot (elapsed-ms watch now-ms) 1000) "s"))

(defn describe
  "One paragraph for one watched spawn."
  [watch state verdict now-ms]
  (let [nm (:name watch)
        where (endpoint watch state)
        took (waited watch now-ms)]
    (case verdict
      :connected
      (str "CIDER session " (pr-str nm) " (" where ") is CONNECTED after " took
           ". Evaluate in it with `code cider eval` and :session_name " (pr-str nm) ".")

      :failed
      (str "CIDER session " (pr-str nm) " (" where ") FAILED after " took ": "
           (or (:reason state) (:status state))
           ". Nothing is listening for an eval — read the *nREPL-" nm
           "* buffer for the cause, then respawn.")

      :overdue
      (str "CIDER session " (pr-str nm) " (" where ") has NOT settled after " took
           "; its last published status is " (pr-str (:status state))
           ". Confirm with `code cider sessions` before evaluating in it.")

      :vanished
      (str "CIDER session " (pr-str nm) " (" where ") never reached the Emacs session"
           " registry within " took ". The spawn did not take; respawn it.")

      :unpublished
      (str "CIDER session " (pr-str nm) " (" where ") has no outcome after " took
           " because the Emacs half is NOT PUBLISHING session state — this says"
           " nothing about the session itself. Ask `code cider sessions` instead,"
           " and expect no further spawn blocks until the publisher is back.")

      :pending
      (str "CIDER session " (pr-str nm) " (" where ") is still starting (" took ")."
           " Wait for its outcome before evaluating in it."))))

(defn rows
  "Watched spawns paired with the state published for them and their verdict.
   PUBLISHED? defaults to true: assume an observer unless told otherwise."
  ([watches states now-ms] (rows watches states now-ms true))
  ([watches states now-ms published?]
   (mapv (fn [[nm watch]]
           (let [state (get states nm)]
             [watch state (verdict watch state now-ms published?)]))
         (sort-by key watches))))

(defn render-block
  "Block body for `[[watch state verdict] ...]`, or nil when there is none."
  [rows now-ms]
  (let [paragraphs (map (fn [[watch state verdict]]
                          (describe watch state verdict now-ms))
                        rows)]
    (when (seq paragraphs)
      (str/join "\n\n" paragraphs))))

(defn settled-watches
  "The name->watch map of ROWS whose verdict is an outcome."
  [rows]
  (into {} (keep (fn [[watch _state verdict]]
                   (when (settled? verdict) [(:name watch) watch])))
        rows))

;; ── shell ───────────────────────────────────────────────────────────────────

(defonce ^:private ledger
  (atom {}))

(defn watches
  "Every spawn whose outcome has not been reported, by name."
  []
  @ledger)

(defn watch!
  "Record WATCH as a spawn whose outcome is owed to the agent. Returns it."
  [watch]
  (swap! ledger assoc (:name watch) watch)
  watch)

(def report-limit
  "How often a settled outcome is put on a response before it is dropped.
   An emitter call is not a delivery — a block can be rendered onto a response
   the agent never reads — so an outcome survives one lost delivery."
  2)

(defn- same-spawn?
  "True when two watches are the same spawn, whatever their report count."
  [a b]
  (and (some? a) (some? b)
       (= (:name a) (:name b))
       (= (:requested-ms a) (:requested-ms b))))

(defn note-reported
  "LEDGER with each REPORTED watch counted once more, and dropped once its
   outcome has been reported `report-limit` times. Pure."
  [ledger reported]
  (reduce-kv (fn [acc nm watch]
               (let [current (get acc nm)]
                 (if-not (same-spawn? watch current)
                   acc
                   (let [reports (inc (or (:reports current) 0))]
                     (if (>= reports report-limit)
                       (dissoc acc nm)
                       (assoc acc nm (assoc current :reports reports)))))))
             ledger
             reported))

(defn forget!
  "Count REPORTED as delivered, dropping each watch whose outcome has now been
   reported often enough. A respawn under the same name is left alone."
  [reported]
  (swap! ledger note-reported reported))

(defn reset-watches!
  "Forget every watch. For tests and addon shutdown."
  []
  (reset! ledger {}))

(def ^:dynamic *root-fn*
  "Publishing-root boundary: (f) -> the directory both halves agree on. Read
   per call, so every caller inherits the seam; tests bind a temp directory."
  attention/root-dir)

(defn- load-dirs
  "Load-path directories for the Emacs half, resolved from the classpath.
   Resolved lazily so this namespace does not load the bridge loader's jar
   extraction at require time."
  []
  (try
    ((requiring-resolve 'hive-emacs.bridge-loader/resolve-elisp-dirs))
    (catch Exception _ [])))

(defn enable-elisp
  "Elisp that loads the Emacs half and starts it publishing under ROOT.
   LOAD-DIRS go on load-path first: an Emacs restarted since the bridge
   loaded does not have them."
  ([root] (enable-elisp root []))
  ([^java.io.File root load-dirs]
   (format "(progn %s(require 'hive-mcp-cider-spawnwatch) (hive-mcp-cider-spawnwatch-enable %s))"
           (apply str (map #(format "(add-to-list 'load-path %s) " (pr-str %)) load-dirs))
           (pr-str (.getAbsolutePath root)))))

(defn enable-in-emacs!
  "Start the Emacs half. EVAL-FN is (fn [code timeout-ms] -> {:success ..}).
   Returns true on success; a failure is logged, never thrown."
  [eval-fn]
  (try
    (let [result (eval-fn (enable-elisp (*root-fn*) (load-dirs)) 5000)]
      (or (boolean (:success result))
          (do (log/warn "cider spawn: enabling the Emacs half failed"
                        {:error (:error result)})
              false)))
    (catch Exception e
      (log/warn "cider spawn: enabling the Emacs half threw" {:error (ex-message e)})
      false)))

(defonce ^:private arming
  (atom false))

(defn arm-publisher!
  "Make sure the Emacs half is publishing, off the caller's thread. At most
   one attempt is in flight. Never blocks, never throws."
  [eval-fn]
  (try
    (when (compare-and-set! arming false true)
      (future
        (try
          (enable-in-emacs! eval-fn)
          (finally
            (reset! arming false))))
      true)
    (catch Exception e
      (log/debug e "cider spawn: arming the publisher failed")
      (reset! arming false)
      false)))

(defn watch-spawn!
  "Railway step over a spawn Result's ok value: record the spawn so its
   outcome is reported, make sure the Emacs half is publishing, and pass the
   value through untouched."
  [eval-fn payload]
  (result/rescue payload
    (when-let [watch (->watch payload (System/currentTimeMillis))]
      (watch! watch)
      (arm-publisher! eval-fn))
    payload))

(defn read-published
  "What the Emacs half has published under ROOT: `:states` by session name, and
   `:published?` for whether any daemon published a registry at all. An absent
   publisher and an empty registry are different facts."
  ([] (read-published (*root-fn*)))
  ([^java.io.File root]
   (let [files (->> (or (.listFiles root) [])
                    (filter #(.isDirectory ^java.io.File %))
                    (keep (fn [^java.io.File dir]
                            (let [f (io/file dir state-file-name)]
                              (when (.isFile f) [(.getName dir) f])))))]
     {:published? (boolean (seq files))
      :states (reduce (fn [m [daemon ^java.io.File f]]
                        (reduce (fn [m state] (assoc m (:name state) state))
                                m
                                (parse-registry daemon (slurp f))))
                      {}
                      files)})))

(defn read-registry
  "Every published session state under ROOT, by session name."
  ([] (:states (read-published)))
  ([^java.io.File root] (:states (read-published root))))

(defn emitter
  "Registered `:block/cider-spawn` emitter: request ctx -> body or nil.
   Nothing is read while no spawn is owed an outcome."
  [_ctx]
  (try
    (let [current (watches)]
      (when (seq current)
        (let [now-ms (System/currentTimeMillis)
              {:keys [states published?]} (read-published)
              rows (rows current states now-ms published?)
              body (render-block rows now-ms)]
          (forget! (settled-watches rows))
          body)))
    (catch Exception e
      (log/debug e "cider spawn: emitter failed; no block")
      nil)))
