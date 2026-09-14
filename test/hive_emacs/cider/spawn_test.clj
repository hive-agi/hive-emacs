(ns hive-emacs.cider.spawn-test
  "The JVM half of the spawn channel: the watch a spawn opens, the verdict a
   published registry settles it with, and the once-only report. The Emacs
   half publishes the file; here the file is the contract."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-dsl.result :as result]
            [hive-emacs.cider.spawn :as spawn]
            [hive-emacs.schema :as schema])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(def ^:private now 1700000000000)

(defn- temp-root
  ^java.io.File []
  (.toFile (Files/createTempDirectory "hive-spawn-test"
                                      (into-array FileAttribute []))))

(defn- publish!
  "Write SESSIONS as one daemon's cider-spawns.json under ROOT."
  [^java.io.File root daemon sessions]
  (let [dir (io/file root daemon)]
    (.mkdirs dir)
    (spit (io/file dir spawn/state-file-name)
          (json/write-str {:server daemon
                           :pid 4242
                           :heartbeat (/ now 1000.0)
                           :sessions sessions}))
    root))

(defn- acknowledgement
  [overrides]
  (json/write-str (merge {:name "dev" :port 7920 :repl-type "clj" :status "starting"}
                         overrides)))

(defn- watch-of
  [overrides]
  (spawn/->watch (acknowledgement overrides) now))

;; ── the watch a spawn opens ─────────────────────────────────────────────────

(deftest spawn-acknowledgement-becomes-a-conforming-watch
  (let [watch (watch-of {})]
    (is (schema/valid? :hive-emacs/cider-spawn-watch watch))
    (is (= {:name "dev" :port 7920 :repl-type "clj" :project-dir nil
            :requested-ms now :deadline-ms spawn/base-deadline-ms}
           watch))))

(deftest an-upgraded-repl-gets-the-longer-deadline
  (testing "cljs and cljel must still settle an upgrade after connecting"
    (is (< spawn/base-deadline-ms (:deadline-ms (watch-of {:repl-type "cljel"}))))
    (is (= (:deadline-ms (watch-of {:repl-type "cljs"}))
           (:deadline-ms (watch-of {:repl-type "cljel"}))))))

(deftest a-payload-that-is-not-a-spawn-acknowledgement-opens-no-watch
  (is (nil? (spawn/->watch "Error: spawn requires a non-blank 'name'" now)))
  (is (nil? (spawn/->watch (json/write-str {:port 7920}) now)))
  (is (nil? (spawn/->watch (json/write-str {:name ""}) now)))
  (is (nil? (spawn/->watch nil now))))

;; ── the registry the Emacs half publishes ───────────────────────────────────

(deftest a-published-row-parses-into-a-conforming-state
  (let [[state] (spawn/parse-registry "server"
                                      (json/write-str
                                       {:heartbeat (/ now 1000.0)
                                        :sessions [{:name "dev" :port 7920
                                                    :repl-type "clj"
                                                    :status "error"
                                                    :reason "Alias :nope not found"}]}))]
    (is (schema/valid? :hive-emacs/cider-spawn-state state))
    (is (= "error" (:status state)))
    (is (= "Alias :nope not found" (:reason state)))
    (is (= "server" (:server state)) "the directory names the daemon when the file does not")))

(deftest an-unreadable-or-off-contract-registry-yields-no-states
  (is (= [] (spawn/parse-registry "server" "{not json")))
  (is (= [] (spawn/parse-registry "server" (json/write-str {:sessions [{:port 7920}]})))
      "a row with no name is off-contract, not a session"))

;; ── the verdict ─────────────────────────────────────────────────────────────

(deftest a-connected-session-settles-the-watch
  (is (= :connected
         (spawn/verdict (watch-of {}) {:status "connected"} (+ now 3000)))))

(deftest every-status-that-cannot-serve-an-eval-is-a-failure
  (testing "including stale: the session connected and then died"
    (doseq [status ["error" "timeout" "stale"]]
      (is (= :failed (spawn/verdict (watch-of {}) {:status status} (+ now 3000)))
          status))))

(deftest a-session-still-on-its-way-is-pending-until-its-deadline
  (let [watch (watch-of {})]
    (is (= :pending (spawn/verdict watch {:status "starting"} (+ now 3000))))
    (is (= :pending (spawn/verdict watch {:status "connecting"} (+ now 3000))))
    (is (= :pending (spawn/verdict watch nil (+ now 3000)))
        "a registry that has not been published yet is not yet a verdict")))

(deftest silence-past-the-deadline-is-a-verdict-of-its-own
  (let [watch (watch-of {})
        late (+ now (:deadline-ms watch) 1)]
    (is (= :overdue (spawn/verdict watch {:status "connecting"} late))
        "the Emacs half knows the session and it never settled")
    (is (= :vanished (spawn/verdict watch nil late))
        "the Emacs half never knew the session at all")))

(deftest silence-from-an-absent-publisher-is-not-evidence-about-the-session
  (let [watch (watch-of {})
        late (+ now (:deadline-ms watch) 1)]
    (is (= :vanished (spawn/verdict watch nil late true))
        "a publisher that IS running and never saw the session is evidence")
    (is (= :unpublished (spawn/verdict watch nil late false))
        "a publisher that is not running is not")
    (is (= :pending (spawn/verdict watch nil (+ now 3000) false))
        "and before the deadline nothing is claimed either way")))

(deftest the-publisher-down-paragraph-refuses-to-blame-the-session
  (let [body (spawn/describe (watch-of {}) nil :unpublished (+ now 95000))]
    (is (str/includes? body "NOT PUBLISHING"))
    (is (str/includes? body "nothing about the session itself"))
    (is (str/includes? body "cider sessions"))
    (is (not (str/includes? body "respawn it"))
        "the :vanished advice would be a guess here")))

(deftest an-empty-root-and-an-unpublished-root-are-different-facts
  (let [empty-root (temp-root)
        published-root (temp-root)]
    (publish! published-root "server" [])
    (is (false? (:published? (spawn/read-published empty-root)))
        "no daemon wrote a registry at all")
    (is (true? (:published? (spawn/read-published published-root)))
        "a daemon wrote one; it simply holds no sessions")
    (is (= {} (:states (spawn/read-published empty-root))))
    (is (= {} (:states (spawn/read-published published-root))))))

(deftest a-watch-outliving-its-deadline-with-no-publisher-reports-the-publisher
  (let [root (temp-root)
        overdue (assoc (watch-of {:name "orphan"})
                       :requested-ms (- (System/currentTimeMillis)
                                        spawn/base-deadline-ms 1))]
    (binding [spawn/*root-fn* (constantly root)]
      (spawn/reset-watches!)
      (spawn/watch! overdue)
      (let [body (spawn/emitter {})]
        (is (str/includes? body "NOT PUBLISHING"))
        (is (not (str/includes? body "The spawn did not take"))
            "nothing was observed, so nothing is claimed about the spawn"))
      (is (seq (spawn/watches))
          "one rendered block is not one delivered block")
      (spawn/emitter {})
      (is (empty? (spawn/watches))
          "dropped once its outcome has been put on two responses"))))

(deftest only-pending-is-unsettled
  (is (not (spawn/settled? :pending)))
  (doseq [verdict [:connected :failed :overdue :vanished]]
    (is (spawn/settled? verdict) (str verdict))))

;; ── what the agent reads ────────────────────────────────────────────────────

(deftest a-failure-names-its-reason-and-says-the-session-is-unusable
  (let [body (spawn/describe (watch-of {}) {:status "error" :reason "Alias :nope not found"}
                             :failed (+ now 4000))]
    (is (str/includes? body "FAILED"))
    (is (str/includes? body "Alias :nope not found"))
    (is (str/includes? body "*nREPL-dev*"))))

(deftest a-connection-says-how-to-use-the-session
  (let [body (spawn/describe (watch-of {}) {:status "connected"} :connected (+ now 4000))]
    (is (str/includes? body "CONNECTED"))
    (is (str/includes? body "7920"))
    (is (str/includes? body "\"dev\""))))

(deftest the-port-the-session-actually-took-wins-over-the-one-requested
  (let [body (spawn/describe (watch-of {}) {:status "connected" :port 7931}
                             :connected (+ now 1000))]
    (is (str/includes? body "7931"))
    (is (not (str/includes? body "7920")))))

(deftest nothing-watched-renders-no-block
  (is (nil? (spawn/render-block [] now))))

;; ── the ledger, end to end over a real published file ───────────────────────

(deftest an-outcome-survives-one-lost-delivery-and-then-stops
  (let [root (temp-root)
        no-emacs (fn [_code _timeout] {:success false :error "no emacs here"})]
    (publish! root "server"
              [{:name "good" :port 7001 :repl-type "clj" :status "connected"}
               {:name "bad" :port 7002 :repl-type "clj" :status "error"
                :reason "classpath boom"}])
    (binding [spawn/*root-fn* (constantly root)]
      (spawn/reset-watches!)
      (doseq [name ["good" "bad" "slow"]]
        (spawn/watch-spawn! no-emacs (acknowledgement {:name name})))
      (let [first-block (spawn/emitter {})]
        (is (str/includes? first-block "CONNECTED"))
        (is (str/includes? first-block "classpath boom"))
        (is (str/includes? first-block "still starting"))
        (is (= #{"good" "bad" "slow"} (set (keys (spawn/watches))))
            "a block an emitter rendered is not yet a block the agent read"))
      (let [second-block (spawn/emitter {})]
        (is (str/includes? second-block "CONNECTED")
            "so the outcome is put on one more response")
        (is (= #{"slow"} (set (keys (spawn/watches))))
            "and only then is it forgotten"))
      (let [third-block (spawn/emitter {})]
        (is (not (str/includes? third-block "CONNECTED")))
        (is (str/includes? third-block "still starting")
            "a pending spawn keeps reporting, however often it is asked"))
      (spawn/reset-watches!)
      (is (nil? (spawn/emitter {}))
          "with nothing owed the emitter reads nothing and says nothing"))))

(deftest a-respawn-under-the-same-name-is-never-dropped-by-the-previous-report
  (let [watch (watch-of {})
        respawn (assoc watch :requested-ms (+ now 60000))]
    (is (= {"dev" respawn}
           (spawn/note-reported {"dev" respawn} {"dev" watch}))
        "the report belongs to the spawn that is gone, not to the one in flight")))

(deftest a-settled-outcome-is-dropped-once-it-has-been-reported-enough
  (let [watch (watch-of {})
        ledger {"dev" watch}
        after-one (spawn/note-reported ledger {"dev" watch})]
    (is (= 1 (get-in after-one ["dev" :reports])))
    (is (schema/valid? :hive-emacs/cider-spawn-watch (get after-one "dev")))
    (is (empty? (spawn/note-reported after-one {"dev" (get after-one "dev")}))
        (str "dropped at " spawn/report-limit " reports"))))

(deftest the-watch-step-passes-the-spawn-payload-through-untouched
  (let [root (temp-root)
        no-emacs (fn [_code _timeout] {:success false :error "no emacs here"})
        payload (acknowledgement {:name "passthrough"})]
    (binding [spawn/*root-fn* (constantly root)]
      (spawn/reset-watches!)
      (is (= payload (spawn/watch-spawn! no-emacs payload)))
      (is (contains? (spawn/watches) "passthrough")))))

(deftest an-err-never-opens-a-watch
  (let [root (temp-root)
        no-emacs (fn [_code _timeout] {:success false :error "no emacs here"})
        failed (result/err :cider/spawn-failed {:message "emacsclient timed out"})]
    (binding [spawn/*root-fn* (constantly root)]
      (spawn/reset-watches!)
      (is (= failed (result/map-ok failed (partial spawn/watch-spawn! no-emacs))))
      (is (empty? (spawn/watches))
          "nothing is owed for a spawn that never ran"))))

(deftest a-watch-step-that-throws-still-yields-the-payload
  (let [exploding (fn [_code _timeout] (throw (ex-info "boom" {})))]
    (spawn/reset-watches!)
    (is (= "unparseable" (spawn/watch-spawn! exploding "unparseable"))
        "the railway carries the spawn's own answer, whatever the watch does")))

;; ── the elisp the JVM half sends ────────────────────────────────────────────

(deftest enabling-the-emacs-half-names-the-root-both-sides-read
  (let [root (temp-root)
        code (spawn/enable-elisp root ["/opt/elisp"])]
    (is (str/includes? code "(require 'hive-mcp-cider-spawnwatch)"))
    (is (str/includes? code (.getAbsolutePath root)))
    (is (str/includes? code "(add-to-list 'load-path \"/opt/elisp\")"))))

(deftest enabling-reports-a-failure-instead-of-throwing
  (let [root (temp-root)]
    (binding [spawn/*root-fn* (constantly root)]
      (is (false? (spawn/enable-in-emacs! (fn [_ _] {:success false :error "no daemon"}))))
      (is (false? (spawn/enable-in-emacs! (fn [_ _] (throw (ex-info "boom" {}))))))
      (is (true? (spawn/enable-in-emacs! (fn [_ _] {:success true :result "t"})))))))
