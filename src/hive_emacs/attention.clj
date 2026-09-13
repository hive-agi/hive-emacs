(ns hive-emacs.attention
  "Emacs is waiting for a human: see it without emacsclient, answer it without
   emacsclient, and say so on every tool response until it is resolved.

   ## Why the channel is a file

   Measured 2026-09-13 (Emacs 31.0.50, throwaway daemon, real tty frame): a
   prompt opened INSIDE an emacsclient eval, which is how `cider spawn` runs
   cider-jack-in, makes every other emacsclient call time out until it is
   answered. Hooks and timers keep running, though. So the Emacs half
   (`hive-mcp-attention.el`, compiled from src/cljel/hive_mcp/attention.cljel)
   writes `<root>/<server-name>/attention.json` from a minibuffer hook plus a
   1s timer, and consumes `answer.json` from the same timer. This namespace is
   the JVM half and never needs Emacs to be responsive.

   ## Layers

   Pure core (parse, classify, render, answer document) is data in, data out
   and carries the policy. The shell below it lists the root, reads files,
   writes the answer atomically and evaluates the one-line enable form.

   ## Policy

   A prompt the human opened with a key (`origin` \"command\", e.g. M-x) is not
   an impediment and never alerts. A prompt whose heartbeat has stopped while
   its Emacs is alive is reported as FROZEN: timers are not running, so an
   answer file would not be consumed either. A prompt whose Emacs is gone is
   ignored."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [hive-emacs.config :as config]
            [hive-emacs.schema :as schema]
            [taoensso.timbre :as log])
  (:import [java.nio.file Files StandardCopyOption]
           [java.nio.file.attribute FileAttribute PosixFilePermissions]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def extension-key
  "Host block-emitter key. The key NAME is the wire tag: ---EMACS-ATTENTION---."
  :block/emacs-attention)

(def state-file-name "attention.json")

(def answer-file-name "answer.json")

(def heartbeat-stale-ms
  "The Emacs timer refreshes every 1000ms; five missed beats means it is not
   running."
  5000)

(def user-origin
  "Origin of a prompt the human opened with a key. Never an impediment."
  "command")

;; ── pure core ───────────────────────────────────────────────────────────────

(defn parse-state
  "JSON text of attention.json -> AttentionState, or nil when unreadable or
   off-contract. `daemon-dir` is the directory name, used when the file omits
   :server."
  [daemon-dir text]
  (try
    (let [raw (json/read-str text :key-fn keyword)
          state (cond-> {:kind (:kind raw)
                         :id (:id raw)
                         :origin (or (:origin raw) "unknown")
                         :prompt (or (:prompt raw) "")
                         :secret? (true? (:secret raw))
                         :server (or (:server raw) daemon-dir)
                         :heartbeat-ms (some-> (:heartbeat raw) (* 1000) long)}
                  (number? (:since raw)) (assoc :since-ms (long (* 1000 (:since raw))))
                  (string? (:input raw)) (assoc :input (:input raw))
                  (integer? (:pid raw)) (assoc :pid (long (:pid raw)))
                  (integer? (:depth raw)) (assoc :depth (long (:depth raw))))]
      (when (schema/valid? :hive-emacs/attention-state state)
        state))
    (catch Exception _ nil)))

(defn liveness
  "`:live`, `:frozen` (Emacs alive, heartbeat stopped) or `:gone`.
   `pid-alive` answers true, false, or nil when the pid is not visible from
   this process (another pid namespace); nil falls back to the heartbeat."
  [{:keys [heartbeat-ms]} now-ms pid-alive]
  (let [fresh? (and heartbeat-ms (< (- now-ms heartbeat-ms) heartbeat-stale-ms))]
    (cond
      (false? pid-alive) :gone
      fresh? :live
      (true? pid-alive) :frozen
      :else :gone)))

(defn actionable?
  "True when this prompt impedes agents: not one the human opened with a key."
  [{:keys [origin]}]
  (not= user-origin origin))

(defn- waited
  [{:keys [since-ms]} now-ms]
  (if since-ms
    (str (max 0 (quot (- now-ms since-ms) 1000)) "s")
    "?s"))

(defn- quoted
  [s]
  (pr-str (str/trim (str/replace (or s "") #"\s+" " "))))

(defn- opened-by
  [origin]
  (case origin
    "server" "an emacsclient eval"
    "timer" "a timer"
    "async" "a process filter or sentinel (e.g. a CIDER connection callback)"
    "command" "the user"
    "an unknown caller"))

(defn- what
  [{:keys [kind prompt]}]
  (case kind
    "debugger" (str "the Emacs DEBUGGER is open: "
                    (quoted (first (str/split-lines (or prompt "")))))
    "read-event" (str "a key prompt " (quoted prompt))
    (str "a minibuffer prompt " (quoted prompt))))

(defn- consequence
  [{:keys [origin]} status]
  (cond
    (= :frozen status)
    "Its timer has STOPPED, so an answer file will not be read either: Emacs is busy in a blocking loop. Ask the user."
    (= "server" origin)
    "Every emacsclient call (emacs, cider, magit tools) times out until it is answered."
    :else
    "Work waiting on it (a spawn, a connection, a load) cannot finish until it is answered."))

(defn- question-kind
  "What a prompt asks for, read from its text: :yes-or-no (typed word),
   :y-or-n (one key) or :text. The keys to suggest follow from it: \"y\" is a
   wrong answer to a completing-read such as \"REPL buffer to reuse:\"."
  [prompt]
  (cond
    (re-find #"\((?:yes or no)\)" (or prompt "")) :yes-or-no
    (re-find #"(?i)\(y or n\)|[\[(]y/n[\])]" (or prompt "")) :y-or-n
    :else :text))

(defn- action
  [{:keys [id kind secret? prompt]} status]
  (cond
    (= :frozen status) nil
    secret? "SECRET prompt (password): do not answer it. Ask the user to type it in Emacs."
    (= "debugger" kind)
    (format "Act: emacs command=\"answer\" id=\"%s\" keys=\"q\" quits the debugger, or ask the user."
            id)
    :else
    (case (question-kind prompt)
      :yes-or-no
      (format "Act: emacs command=\"answer\" id=\"%s\" keys=\"yes RET\" (or \"no RET\"; \"C-g\" aborts), or ask the user."
              id)
      :y-or-n
      (format "Act: emacs command=\"answer\" id=\"%s\" keys=\"y\" (or \"n\"; \"C-g\" aborts), or ask the user."
              id)
      :text
      (if (= "read-event" kind)
        (format "Act: emacs command=\"answer\" id=\"%s\" keys=\"<a key the prompt offers>\" (kbd syntax, \"C-g\" aborts), or ask the user."
                id)
        (format "Act: emacs command=\"answer\" id=\"%s\" keys=\"RET\" accepts the default, keys=\"<text> RET\" types a value (kbd syntax: SPC is a space, \"C-g\" aborts), or ask the user."
                id)))))

(defn describe
  "One paragraph for one waiting prompt."
  [state status now-ms]
  (->> [(format "Emacs daemon %s is WAITING FOR INPUT (%s, %s): %s, opened by %s."
                (quoted (:server state))
                (waited state now-ms)
                (name status)
                (what state)
                (opened-by (:origin state)))
        (consequence state status)
        (action state status)]
       (remove nil?)
       (str/join "\n")))

(defn render-block
  "Block body for `[[state status] ...]`, or nil when nothing impedes agents."
  [states+status now-ms]
  (let [paragraphs (for [[state status] states+status
                         :when (and (actionable? state) (not= :gone status))]
                     (describe state status now-ms))]
    (when (seq paragraphs)
      (str/join "\n\n" paragraphs))))

(defn answer-document
  "answer.json body for prompt ID. KEY-STRING is kbd syntax."
  [id key-string]
  (json/write-str {:id id :keys key-string}))

(defn select-target
  "The state an answer addresses, or an error map. `daemon` and `id` narrow;
   with neither, the single actionable prompt is chosen and several are an
   ambiguity the caller must resolve."
  [states {:keys [daemon id]}]
  (let [candidates (cond->> (filter actionable? states)
                     daemon (filter #(= daemon (:server %)))
                     id (filter #(= id (:id %))))]
    (cond
      (empty? candidates)
      {:error :attention/nothing-waiting
       :message (str "No impeding Emacs prompt"
                     (when daemon (str " on daemon " daemon))
                     (when id (str " with id " id)) ".")}

      (next candidates)
      {:error :attention/ambiguous
       :message "Several Emacs prompts are waiting; pass id."
       :ids (mapv :id candidates)}

      (:secret? (first candidates))
      {:error :attention/secret
       :message "The waiting prompt is secret (a password). Only the user may answer it."}

      :else
      {:ok (first candidates)})))

;; ── shell ───────────────────────────────────────────────────────────────────

(defn root-dir
  "Root both halves agree on; Emacs is handed this path by `enable-elisp`."
  ^java.io.File []
  (let [{:keys [attention-dir xdg-runtime-dir]} (config/resolve!)]
    (cond
      (not (str/blank? attention-dir)) (io/file attention-dir)
      (and (not (str/blank? xdg-runtime-dir)) (.isDirectory (io/file xdg-runtime-dir)))
      (io/file xdg-runtime-dir "hive-emacs")
      :else
      (io/file (System/getProperty "java.io.tmpdir")
               (str "hive-emacs-" (System/getProperty "user.name"))))))

(defn- pid-alive
  [pid]
  (when pid
    (let [handle (java.lang.ProcessHandle/of (long pid))]
      (when (.isPresent handle)
        (.isAlive ^java.lang.ProcessHandle (.get handle))))))

(defn read-states
  "Every readable AttentionState under ROOT, each paired with its liveness."
  ([] (read-states (root-dir) (System/currentTimeMillis)))
  ([^java.io.File root now-ms]
   (->> (or (.listFiles root) [])
        (filter #(.isDirectory ^java.io.File %))
        (keep (fn [^java.io.File dir]
                (let [f (io/file dir state-file-name)]
                  (when (.isFile f)
                    (when-let [state (parse-state (.getName dir) (slurp f))]
                      [state (liveness state now-ms (pid-alive (:pid state)))])))))
        vec)))

(defn emitter
  "Registered `:block/emacs-attention` emitter: request ctx -> body or nil.
   A directory listing and a small read per call; never touches emacsclient."
  [_ctx]
  (try
    (let [root (root-dir)]
      (when (.isDirectory root)
        (render-block (read-states root (System/currentTimeMillis))
                      (System/currentTimeMillis))))
    (catch Exception e
      (log/debug e "attention: emitter failed; no block")
      nil)))

(defn timeout-hint
  "Sentence appended to an emacsclient timeout when a prompt explains it."
  []
  (try
    (let [now (System/currentTimeMillis)]
      (render-block (read-states (root-dir) now) now))
    (catch Exception _ nil)))

(defn- write-private!
  "Write TEXT to FILE via a 0600 temp file and an atomic rename."
  [^java.io.File file ^String text]
  (let [dir (.toPath (.getParentFile file))
        owner-only (PosixFilePermissions/asFileAttribute
                    (PosixFilePermissions/fromString "rw-------"))
        tmp (Files/createTempFile dir ".answer" ".tmp"
                                  (into-array FileAttribute [owner-only]))]
    (spit (.toFile tmp) text)
    (Files/move tmp (.toPath file)
                (into-array java.nio.file.CopyOption
                            [StandardCopyOption/ATOMIC_MOVE
                             StandardCopyOption/REPLACE_EXISTING]))))

(defn- still-waiting?
  [root server id]
  (some (fn [[state _]] (and (= server (:server state)) (= id (:id state))))
        (read-states root (System/currentTimeMillis))))

(defn answer!
  "Answer a waiting prompt with KEYS (kbd syntax). Needs no emacsclient.
   Waits up to `wait-ms` for Emacs to consume the answer and close the prompt."
  [{key-string :keys :keys [wait-ms] :or {wait-ms 3000} :as opts}]
  (let [root (root-dir)
        now (System/currentTimeMillis)
        states+status (read-states root now)
        live (filter (fn [[_ status]] (= :live status)) states+status)
        frozen (filter (fn [[_ status]] (= :frozen status)) states+status)
        target (select-target (map first live) opts)]
    (cond
      (str/blank? key-string)
      {:error :attention/keys-required :message "keys is required, e.g. \"y\" or \"RET\"."}

      (and (:error target) (seq frozen) (empty? live))
      {:error :attention/frozen
       :message "Emacs is waiting but its timer is not running, so an answer cannot be delivered. Ask the user."}

      (:error target) target

      :else
      (let [{:keys [server id]} (:ok target)
            file (io/file root server answer-file-name)]
        (write-private! file (answer-document id key-string))
        (let [deadline (+ (System/currentTimeMillis) wait-ms)]
          (loop []
            (let [waiting? (still-waiting? root server id)]
              (cond
                (not waiting?)
                {:ok {:answered? true :daemon server :id id :keys key-string}}

                (> (System/currentTimeMillis) deadline)
                {:ok {:answered? false :daemon server :id id :keys key-string
                      :note (if (.exists file)
                              "Answer not consumed yet; Emacs reads it once per second."
                              "Answer consumed but the prompt is still open: the keys may not complete it (try \"RET\") or Emacs refused them.")}}

                :else
                (do (Thread/sleep 100) (recur))))))))))

(defn enable-elisp
  "Elisp that loads the Emacs half, starts it publishing under ROOT and
   answers the Emacs pid. LOAD-DIRS go on load-path first: an Emacs that was
   restarted since the bridge loaded does not have them."
  ([root] (enable-elisp root []))
  ([^java.io.File root load-dirs]
   (format "(progn %s(require 'hive-mcp-attention) (hive-mcp-attention-enable %s) (emacs-pid))"
           (apply str (map #(format "(add-to-list 'load-path %s) " (pr-str %)) load-dirs))
           (pr-str (.getAbsolutePath root)))))

;; ── upkeep: keep the Emacs half publishing across Emacs restarts ────────────
;;
;; The publisher lives in Emacs, so an Emacs restart silently stops it and no
;; block ever appears again. Asking Emacs on every tool call would put an
;; emacsclient round-trip on every response, and would hang exactly when a
;; prompt blocks emacsclient. Instead the enable form answers the Emacs pid,
;; and each emitter call checks that pid locally. Only a pid that is gone (or
;; was never learned) schedules a re-enable, in the background and at most
;; once per `upkeep-retry-ms`.

(def upkeep-retry-ms
  "Minimum gap between two re-enable attempts."
  60000)

(defonce ^:private upkeep
  (atom {:pid nil :last-attempt-ms nil :in-flight? false}))

(defn upkeep-due?
  "True when a re-enable should start now. PID-ALIVE is true, false, or nil
   when the recorded pid is not visible from this process (another pid
   namespace); nil is treated as alive, never as a restart."
  [{:keys [pid last-attempt-ms in-flight?]} now-ms pid-alive]
  (and (not in-flight?)
       (or (nil? pid) (false? pid-alive))
       (or (nil? last-attempt-ms)
           (>= (- now-ms last-attempt-ms) upkeep-retry-ms))))

(defn reset-upkeep!
  "Forget the recorded publisher. For tests and addon shutdown."
  []
  (reset! upkeep {:pid nil :last-attempt-ms nil :in-flight? false})
  nil)

(defn- load-dirs
  "Load-path directories for the Emacs half, resolved from the classpath.
   Resolved lazily so this namespace does not load the bridge loader's jar
   extraction at require time."
  []
  (try
    ((requiring-resolve 'hive-emacs.bridge-loader/resolve-elisp-dirs))
    (catch Exception _ [])))

(defn enable-in-emacs!
  "Start the Emacs half. EVAL-FN is (fn [code timeout-ms] -> {:success ..}).
   Returns true on success and records the Emacs pid for upkeep; a failure is
   logged, never thrown."
  [eval-fn]
  (try
    (let [root (root-dir)
          result (eval-fn (enable-elisp root (load-dirs)) 5000)]
      (if (:success result)
        (do (swap! upkeep assoc :pid (some-> (:result result) str str/trim parse-long))
            true)
        (do (log/warn "attention: enabling the Emacs half failed" {:error (:error result)})
            false)))
    (catch Exception e
      (log/warn "attention: enabling the Emacs half threw" {:error (ex-message e)})
      false)))

(defn keep-publishing!
  "Re-enable the Emacs half in the background when its Emacs is gone.
   Returns true when an attempt was started. Never blocks, never throws."
  [eval-fn]
  (try
    (let [now (System/currentTimeMillis)
          before @upkeep]
      (when (and (upkeep-due? before now (pid-alive (:pid before)))
                 (compare-and-set! upkeep before
                                   (assoc before :in-flight? true :last-attempt-ms now)))
        (future
          (try
            (when-not (enable-in-emacs! eval-fn)
              (swap! upkeep assoc :pid nil))
            (finally
              (swap! upkeep assoc :in-flight? false))))
        true))
    (catch Exception e
      (log/debug e "attention: upkeep check failed")
      false)))

(defn emitter-with-upkeep
  "The block emitter plus upkeep: every call also makes sure the Emacs half is
   still publishing (see `keep-publishing!`)."
  [eval-fn]
  (fn [ctx]
    (keep-publishing! eval-fn)
    (emitter ctx)))
