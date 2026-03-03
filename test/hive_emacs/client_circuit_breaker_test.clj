(ns hive-emacs.client-circuit-breaker-test
  "Tests for the 3-state circuit breaker in hive-emacs.client.

   The circuit breaker is the PRIMARY fix for the GC death spiral:
   previously the breaker was SET on Emacs failure but NEVER CHECKED,
   so each heartbeat spawned infinite futures -> OOM.

   State machine transitions tested:
   1. :closed  -> :open      on daemon death detection
   2. :open    -> :half-open  after backoff period elapses
   3. :half-open -> :closed   on successful probe call
   4. :half-open -> :open     on failed probe call
   5. :open    -> :open       exponential backoff doubling
   6. :closed  stays :closed  on successful calls
   7. Calls blocked when :open and backoff hasn't elapsed"
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [hive-emacs.client :as client]))

;;; =============================================================================
;;; Test Fixtures
;;; =============================================================================

(defn reset-breaker-fixture
  "Reset circuit breaker to :closed before each test."
  [f]
  (client/reset-circuit-breaker!)
  (f)
  (client/reset-circuit-breaker!))

(use-fixtures :each reset-breaker-fixture)

;;; =============================================================================
;;; Initial State Tests
;;; =============================================================================

(deftest initial-state-is-closed
  (testing "Circuit breaker starts in :closed state"
    (let [cb (client/circuit-breaker-state)]
      (is (= :closed (:state cb)))
      (is (nil? (:tripped-at cb)))
      (is (= 0 (:crash-count cb)))
      (is (nil? (:last-error cb)))
      (is (nil? (:last-tag cb))))))

;;; =============================================================================
;;; State Machine Transition Tests (unit-level, no real emacsclient)
;;; =============================================================================

(deftest closed-to-open-on-failure
  (testing "Circuit transitions :closed -> :open when daemon death is detected"
    ;; Directly trip the breaker (simulating what eval-elisp-with-timeout does
    ;; when it detects a daemon death pattern)
    (#'client/trip-breaker! "Connection refused" :connection-refused)

    (let [cb (client/circuit-breaker-state)]
      (is (= :open (:state cb)))
      (is (some? (:tripped-at cb)))
      (is (= 1 (:crash-count cb)))
      (is (= "Connection refused" (:last-error cb)))
      (is (= :connection-refused (:last-tag cb)))
      (is (= client/initial-backoff-ms (:backoff-ms cb))))))

(deftest open-stays-open-with-exponential-backoff
  (testing "Repeated failures in :open state double the backoff"
    ;; First failure: backoff = initial (1000ms)
    (#'client/trip-breaker! "Connection refused" :connection-refused)
    (is (= client/initial-backoff-ms (:backoff-ms (client/circuit-breaker-state))))

    ;; Second failure while open: backoff doubles
    (#'client/trip-breaker! "Connection refused" :connection-refused)
    (is (= (* 2 client/initial-backoff-ms) (:backoff-ms (client/circuit-breaker-state))))

    ;; Third failure: doubles again
    (#'client/trip-breaker! "Connection refused" :connection-refused)
    (is (= (* 4 client/initial-backoff-ms) (:backoff-ms (client/circuit-breaker-state))))

    ;; Crash count accumulates
    (is (= 3 (:crash-count (client/circuit-breaker-state))))))

(deftest backoff-caps-at-max
  (testing "Exponential backoff caps at max-backoff-ms (60s)"
    ;; Trip many times to exceed max
    (dotimes [_ 20]
      (#'client/trip-breaker! "dead" :socket-not-found))

    (let [cb (client/circuit-breaker-state)]
      (is (<= (:backoff-ms cb) client/max-backoff-ms))
      (is (= client/max-backoff-ms (:backoff-ms cb))))))

(deftest open-to-half-open-after-backoff
  (testing "Circuit transitions :open -> :half-open after backoff elapses"
    ;; Trip the breaker
    (#'client/trip-breaker! "dead" :socket-not-found)
    (is (= :open (:state (client/circuit-breaker-state))))

    ;; Manually set tripped-at to the past so backoff has elapsed
    (swap! @#'client/circuit-breaker assoc
           :tripped-at (- (System/currentTimeMillis) (* 2 client/initial-backoff-ms)))

    ;; Now maybe-half-open! should succeed
    (let [transitioned? (#'client/maybe-half-open!)]
      (is (true? transitioned?))
      (is (= :half-open (:state (client/circuit-breaker-state)))))))

(deftest open-stays-open-during-backoff
  (testing "Circuit stays :open if backoff hasn't elapsed"
    (#'client/trip-breaker! "dead" :socket-not-found)
    (is (= :open (:state (client/circuit-breaker-state))))

    ;; Don't manipulate time — backoff is 1000ms and we just tripped
    ;; So maybe-half-open! should return false
    (let [transitioned? (#'client/maybe-half-open!)]
      (is (false? transitioned?))
      (is (= :open (:state (client/circuit-breaker-state)))))))

(deftest half-open-to-closed-on-success
  (testing "Circuit transitions :half-open -> :closed on successful call"
    ;; Get to half-open state
    (#'client/trip-breaker! "dead" :socket-not-found)
    (swap! @#'client/circuit-breaker assoc
           :tripped-at (- (System/currentTimeMillis) (* 2 client/initial-backoff-ms)))
    (#'client/maybe-half-open!)
    (is (= :half-open (:state (client/circuit-breaker-state))))

    ;; Simulate successful recovery
    (#'client/recover-breaker!)

    (let [cb (client/circuit-breaker-state)]
      (is (= :closed (:state cb)))
      (is (some? (:recovery-at cb)))
      (is (= client/initial-backoff-ms (:backoff-ms cb))
          "Backoff resets to initial on recovery"))))

(deftest half-open-to-open-on-failure
  (testing "Circuit transitions :half-open -> :open on failed probe"
    ;; Get to half-open state
    (#'client/trip-breaker! "dead" :socket-not-found)
    (swap! @#'client/circuit-breaker assoc
           :tripped-at (- (System/currentTimeMillis) (* 2 client/initial-backoff-ms)))
    (#'client/maybe-half-open!)
    (is (= :half-open (:state (client/circuit-breaker-state))))

    ;; Probe fails
    (#'client/trip-breaker! "still dead" :connection-refused)

    (let [cb (client/circuit-breaker-state)]
      (is (= :open (:state cb)))
      (is (= "still dead" (:last-error cb)))
      ;; From half-open, backoff resets to initial (not doubled)
      (is (= client/initial-backoff-ms (:backoff-ms cb))))))

;;; =============================================================================
;;; Guard Check Tests (check-circuit-breaker)
;;; =============================================================================

(deftest guard-proceeds-when-closed
  (testing "check-circuit-breaker returns :proceed when closed"
    (is (= :proceed (#'client/check-circuit-breaker)))))

(deftest guard-blocks-when-open-in-backoff
  (testing "check-circuit-breaker returns :blocked when open and in backoff"
    (#'client/trip-breaker! "dead" :socket-not-found)
    ;; Just tripped, backoff hasn't elapsed
    (is (= :blocked (#'client/check-circuit-breaker)))))

(deftest guard-proceeds-when-open-backoff-elapsed
  (testing "check-circuit-breaker returns :proceed when open but backoff elapsed (transitions to half-open)"
    (#'client/trip-breaker! "dead" :socket-not-found)
    ;; Set tripped-at to past
    (swap! @#'client/circuit-breaker assoc
           :tripped-at (- (System/currentTimeMillis) (* 2 client/initial-backoff-ms)))
    (is (= :proceed (#'client/check-circuit-breaker)))
    (is (= :half-open (:state (client/circuit-breaker-state))))))

(deftest guard-proceeds-when-half-open
  (testing "check-circuit-breaker returns :proceed when half-open"
    ;; Manually set to half-open
    (swap! @#'client/circuit-breaker assoc :state :half-open)
    (is (= :proceed (#'client/check-circuit-breaker)))))

;;; =============================================================================
;;; Integration Tests (eval-elisp-with-timeout with circuit breaker)
;;; =============================================================================

(deftest eval-returns-circuit-open-sentinel-when-blocked
  (testing "eval-elisp-with-timeout returns sentinel when circuit is open"
    ;; Trip the breaker
    (#'client/trip-breaker! "Connection refused" :connection-refused)

    ;; Call should be blocked immediately — no process spawned
    (let [result (client/eval-elisp-with-timeout "t" 5000)]
      (is (false? (:success result)))
      (is (true? (:circuit-open result)))
      (is (= 0 (:duration-ms result))
          "Duration should be 0 — no process was spawned")
      (is (re-find #"Circuit breaker open" (:error result))))))

(deftest eval-elisp!-returns-circuit-open-map
  (testing "eval-elisp! returns {:error :circuit-open ...} when breaker is open"
    (#'client/trip-breaker! "dead" :socket-not-found)
    (let [result (client/eval-elisp! "t")]
      (is (= :circuit-open (:error result)))
      (is (some? (:msg result))))))

;;; =============================================================================
;;; Reset Tests
;;; =============================================================================

(deftest reset-circuit-breaker-restores-closed
  (testing "reset-circuit-breaker! restores :closed state"
    (#'client/trip-breaker! "dead" :socket-not-found)
    (#'client/trip-breaker! "dead" :socket-not-found)
    (is (= :open (:state (client/circuit-breaker-state))))

    (client/reset-circuit-breaker!)

    (let [cb (client/circuit-breaker-state)]
      (is (= :closed (:state cb)))
      (is (= 0 (:crash-count cb)))
      (is (nil? (:last-error cb)))
      (is (= client/initial-backoff-ms (:backoff-ms cb))))))

;;; =============================================================================
;;; Daemon Death Pattern Detection Tests
;;; =============================================================================

(deftest detect-daemon-death-patterns
  (testing "detect-daemon-death matches known error patterns"
    (is (some? (#'client/detect-daemon-death "can't find socket /tmp/emacs/server")))
    (is (some? (#'client/detect-daemon-death "No such file or directory")))
    (is (some? (#'client/detect-daemon-death "Connection refused")))
    (is (some? (#'client/detect-daemon-death "connection reset by peer")))
    (is (some? (#'client/detect-daemon-death "server did not respond")))
    (is (some? (#'client/detect-daemon-death "socket not available"))))

  (testing "detect-daemon-death returns nil for non-death errors"
    (is (nil? (#'client/detect-daemon-death "some random error")))
    (is (nil? (#'client/detect-daemon-death nil)))
    (is (nil? (#'client/detect-daemon-death "")))))

(deftest detect-daemon-death-returns-tag
  (testing "detect-daemon-death returns correct tag"
    (let [[matched? tag] (#'client/detect-daemon-death "Connection refused")]
      (is (true? matched?))
      (is (= :connection-refused tag)))))

;;; =============================================================================
;;; Full Lifecycle Scenario Tests
;;; =============================================================================

(deftest full-lifecycle-closed-open-halfopen-closed
  (testing "Complete lifecycle: closed -> open -> half-open -> closed"
    ;; 1. Start closed
    (is (= :closed (:state (client/circuit-breaker-state))))

    ;; 2. Failure trips to open
    (#'client/trip-breaker! "Connection refused" :connection-refused)
    (is (= :open (:state (client/circuit-breaker-state))))

    ;; 3. Wait for backoff (simulate by adjusting tripped-at)
    (swap! @#'client/circuit-breaker assoc
           :tripped-at (- (System/currentTimeMillis) (* 2 client/initial-backoff-ms)))

    ;; 4. Guard check transitions to half-open
    (is (= :proceed (#'client/check-circuit-breaker)))
    (is (= :half-open (:state (client/circuit-breaker-state))))

    ;; 5. Successful probe recovers to closed
    (#'client/recover-breaker!)
    (is (= :closed (:state (client/circuit-breaker-state))))
    (is (some? (:recovery-at (client/circuit-breaker-state))))))

(deftest full-lifecycle-closed-open-halfopen-open-retry
  (testing "Complete lifecycle: closed -> open -> half-open -> open (probe failure, retry)"
    ;; 1. Start closed
    (is (= :closed (:state (client/circuit-breaker-state))))

    ;; 2. Failure trips to open with initial backoff
    (#'client/trip-breaker! "dead" :socket-not-found)
    (is (= :open (:state (client/circuit-breaker-state))))
    (is (= client/initial-backoff-ms (:backoff-ms (client/circuit-breaker-state))))

    ;; 3. Backoff elapses, transition to half-open
    (swap! @#'client/circuit-breaker assoc
           :tripped-at (- (System/currentTimeMillis) (* 2 client/initial-backoff-ms)))
    (#'client/maybe-half-open!)
    (is (= :half-open (:state (client/circuit-breaker-state))))

    ;; 4. Probe fails -> back to open with reset backoff
    (#'client/trip-breaker! "still dead" :connection-refused)
    (is (= :open (:state (client/circuit-breaker-state))))
    (is (= client/initial-backoff-ms (:backoff-ms (client/circuit-breaker-state)))
        "Backoff resets to initial from half-open failure")

    ;; 5. Second attempt: wait, transition to half-open, succeed
    (swap! @#'client/circuit-breaker assoc
           :tripped-at (- (System/currentTimeMillis) (* 2 client/initial-backoff-ms)))
    (#'client/maybe-half-open!)
    (is (= :half-open (:state (client/circuit-breaker-state))))
    (#'client/recover-breaker!)
    (is (= :closed (:state (client/circuit-breaker-state))))))

(deftest crash-count-accumulates-across-cycles
  (testing "Crash count accumulates across open/half-open/open cycles"
    ;; Trip 3 times
    (#'client/trip-breaker! "err1" :connection-refused)
    (#'client/trip-breaker! "err2" :connection-refused)
    (#'client/trip-breaker! "err3" :connection-refused)
    (is (= 3 (:crash-count (client/circuit-breaker-state))))

    ;; Recover
    (#'client/recover-breaker!)
    ;; Crash count persists through recovery
    (is (= 3 (:crash-count (client/circuit-breaker-state))))

    ;; Trip again
    (#'client/trip-breaker! "err4" :connection-refused)
    (is (= 4 (:crash-count (client/circuit-breaker-state))))))

(comment
  ;; Run tests from REPL
  ;; (clojure.test/run-tests 'hive-emacs.client-circuit-breaker-test)
  )
