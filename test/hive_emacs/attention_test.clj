(ns hive-emacs.attention-test
  "The JVM half of the attention channel: parse, policy, render, and the
   out-of-band answer. The Emacs half is exercised against a real daemon by
   the e2e recorded in memory; here the file is the contract."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-emacs.attention :as attention]
            [hive-emacs.schema :as schema])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute PosixFilePermission]))

(def ^:private now 1789309000000)

(defn- state-json
  [overrides]
  (json/write-str
   (merge {:kind "minibuffer"
           :id "4242-1"
           :origin "server"
           :prompt "Reuse dead REPL? (y or n) "
           :input ""
           :secret false
           :server "server"
           :pid 4242
           :since (/ (- now 12000) 1000.0)
           :heartbeat (/ (- now 500) 1000.0)
           :depth 1}
          overrides)))

(defn- parsed
  ([] (parsed {}))
  ([overrides] (attention/parse-state "server" (state-json overrides))))

(defn- temp-root
  ^java.io.File []
  (.toFile (Files/createTempDirectory "hive-attention-test"
                                      (into-array FileAttribute []))))

(defn- publish!
  [^java.io.File root daemon overrides]
  (let [dir (io/file root daemon)]
    (.mkdirs dir)
    (spit (io/file dir attention/state-file-name)
          (state-json (merge {:server daemon} overrides)))))

(deftest parse-state-reads-the-emacs-wire-shape
  (let [state (parsed)]
    (is (schema/valid? :hive-emacs/attention-state state))
    (is (= "minibuffer" (:kind state)))
    (is (= "server" (:origin state)))
    (is (false? (:secret? state)))
    (is (= (- now 500) (:heartbeat-ms state)))
    (is (= (- now 12000) (:since-ms state)))
    (is (= 4242 (:pid state)))))

(deftest parse-state-rejects-garbage-rather-than-inventing-a-prompt
  (is (nil? (attention/parse-state "server" "{not json")))
  (is (nil? (attention/parse-state "server" (json/write-str {:kind "minibuffer"}))))
  (is (nil? (parsed {:origin "martian"}))))

(deftest a-null-input-from-a-secret-prompt-is-absent-not-empty
  (let [state (parsed {:secret true :input nil})]
    (is (true? (:secret? state)))
    (is (not (contains? state :input)))))

(deftest liveness-distinguishes-live-frozen-and-gone
  (let [fresh (parsed)
        stale (parsed {:heartbeat (/ (- now 60000) 1000.0)})]
    (testing "fresh heartbeat is live whatever the pid visibility"
      (is (= :live (attention/liveness fresh now true)))
      (is (= :live (attention/liveness fresh now nil))))
    (testing "a dead pid is gone even with a fresh heartbeat"
      (is (= :gone (attention/liveness fresh now false))))
    (testing "stale heartbeat on a live Emacs is frozen: timers stopped"
      (is (= :frozen (attention/liveness stale now true))))
    (testing "stale heartbeat with an invisible pid cannot be told from a crash"
      (is (= :gone (attention/liveness stale now nil))))))

(deftest the-human-typing-m-x-never-alerts-agents
  (is (not (attention/actionable? (parsed {:origin "command"}))))
  (doseq [origin ["server" "timer" "async" "unknown"]]
    (is (attention/actionable? (parsed {:origin origin})) origin)))

(deftest render-block-says-what-blocks-and-how-to-act
  (let [body (attention/render-block [[(parsed) :live]] now)]
    (is (str/includes? body "WAITING FOR INPUT"))
    (is (str/includes? body "Reuse dead REPL? (y or n)"))
    (is (str/includes? body "12s"))
    (is (str/includes? body "an emacsclient eval"))
    (is (str/includes? body "Every emacsclient call"))
    (is (str/includes? body "command=\"answer\" id=\"4242-1\""))))

(deftest render-block-is-silent-when-nothing-impedes
  (is (nil? (attention/render-block [] now)))
  (is (nil? (attention/render-block [[(parsed {:origin "command"}) :live]] now)))
  (is (nil? (attention/render-block [[(parsed) :gone]] now))))

(deftest a-secret-prompt-is-handed-to-the-user
  (let [body (attention/render-block [[(parsed {:secret true :input nil
                                                :prompt "Password: "}) :live]]
                                     now)]
    (is (str/includes? body "SECRET"))
    (is (not (str/includes? body "command=\"answer\"")))))

(deftest a-frozen-emacs-offers-no-answer-it-could-not-read
  (let [body (attention/render-block [[(parsed) :frozen]] now)]
    (is (str/includes? body "STOPPED"))
    (is (not (str/includes? body "command=\"answer\"")))))

(deftest the-debugger-is-quit-with-q
  (let [body (attention/render-block
              [[(parsed {:kind "debugger"
                         :prompt "Debugger entered--Lisp error: (void-function foo)\n  (foo)"}) :live]]
              now)]
    (is (str/includes? body "DEBUGGER"))
    (is (str/includes? body "void-function foo"))
    (is (str/includes? body "keys=\"q\""))))

(deftest select-target-refuses-to-guess
  (let [a (parsed {:id "1-1" :server "a"})
        b (parsed {:id "2-1" :server "b"})
        user (parsed {:id "3-1" :origin "command"})]
    (is (= a (:ok (attention/select-target [a user] {}))))
    (is (= :attention/ambiguous (:error (attention/select-target [a b] {}))))
    (is (= b (:ok (attention/select-target [a b] {:id "2-1"}))))
    (is (= a (:ok (attention/select-target [a b] {:daemon "a"}))))
    (is (= :attention/nothing-waiting (:error (attention/select-target [user] {}))))
    (is (= :attention/secret
           (:error (attention/select-target [(parsed {:secret true :input nil})] {}))))))

(deftest read-states-and-emitter-work-from-disk-alone
  (let [root (temp-root)]
    (publish! root "server" {:pid nil :heartbeat (/ (System/currentTimeMillis) 1000.0)})
    (publish! root "work" {:id "9-9" :origin "command"
                           :pid nil :heartbeat (/ (System/currentTimeMillis) 1000.0)})
    (with-redefs [attention/root-dir (constantly root)]
      (let [states (attention/read-states root (System/currentTimeMillis))]
        (is (= 2 (count states)))
        (is (every? #(= :live (second %)) states)))
      (let [body (attention/emitter {:tool-name "code"})]
        (is (str/includes? body "Reuse dead REPL"))
        (is (not (str/includes? body "9-9")) "the user's own prompt stays out")))))

(deftest emitter-is-nil-without-a-root
  (with-redefs [attention/root-dir (constantly (io/file "/nonexistent/hive-attention"))]
    (is (nil? (attention/emitter {})))))

(deftest answer-writes-a-private-file-emacs-can-trust
  (let [root (temp-root)]
    (publish! root "server" {:pid nil :heartbeat (/ (System/currentTimeMillis) 1000.0)})
    (with-redefs [attention/root-dir (constantly root)]
      (let [result (attention/answer! {:keys "y" :wait-ms 150})
            file (io/file root "server" attention/answer-file-name)]
        (is (false? (get-in result [:ok :answered?])) "no Emacs consumed it")
        (is (= {:id "4242-1" :keys "y"}
               (json/read-str (slurp file) :key-fn keyword)))
        (is (= #{PosixFilePermission/OWNER_READ PosixFilePermission/OWNER_WRITE}
               (Files/getPosixFilePermissions (.toPath file)
                                               (into-array java.nio.file.LinkOption []))))))))

(deftest answer-reports-success-once-emacs-closes-the-prompt
  (let [root (temp-root)
        state-file (io/file root "server" attention/state-file-name)]
    (publish! root "server" {:pid nil :heartbeat (/ (System/currentTimeMillis) 1000.0)})
    (with-redefs [attention/root-dir (constantly root)]
      ;; Stand-in for the Emacs timer: consume the answer, close the prompt.
      (future
        (loop [n 0]
          (if (.exists (io/file root "server" attention/answer-file-name))
            (do (io/delete-file (io/file root "server" attention/answer-file-name))
                (io/delete-file state-file))
            (when (< n 100) (Thread/sleep 20) (recur (inc n))))))
      (is (= {:answered? true :daemon "server" :id "4242-1" :keys "y"}
             (:ok (attention/answer! {:keys "y" :wait-ms 3000})))))))

(deftest answer-refuses-what-it-must-not-send
  (let [root (temp-root)]
    (with-redefs [attention/root-dir (constantly root)]
      (is (= :attention/keys-required (:error (attention/answer! {:keys ""}))))
      (is (= :attention/nothing-waiting (:error (attention/answer! {:keys "y"}))))
      (publish! root "server" {:secret true :input nil :pid nil
                               :heartbeat (/ (System/currentTimeMillis) 1000.0)})
      (is (= :attention/secret (:error (attention/answer! {:keys "y"}))))
      (is (not (.exists (io/file root "server" attention/answer-file-name)))))))

(deftest answer-to-a-frozen-emacs-says-so
  (let [root (temp-root)
        self-pid (.pid (java.lang.ProcessHandle/current))]
    (publish! root "server" {:pid self-pid :heartbeat (/ (- (System/currentTimeMillis) 60000) 1000.0)})
    (with-redefs [attention/root-dir (constantly root)]
      (is (= :attention/frozen (:error (attention/answer! {:keys "y"})))))))

(deftest enable-elisp-hands-emacs-the-jvm-root
  (is (= "(progn (require 'hive-mcp-attention) (hive-mcp-attention-enable \"/run/user/1000/hive-emacs\"))"
         (attention/enable-elisp (io/file "/run/user/1000/hive-emacs")))))

(deftest enable-in-emacs-never-throws
  (is (true? (attention/enable-in-emacs! (fn [_ _] {:success true}))))
  (is (false? (attention/enable-in-emacs! (fn [_ _] {:success false :error "boom"}))))
  (is (false? (attention/enable-in-emacs! (fn [_ _] (throw (ex-info "down" {})))))))
