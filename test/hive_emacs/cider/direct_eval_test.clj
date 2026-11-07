(ns hive-emacs.cider.direct-eval-test
  "Direct nREPL eval against a real in-process nREPL server: value, output,
   errors, the deadline (a form slower than the old 5 s emacsclient cap
   finishes; one slower than its own deadline is interrupted) and an
   unreachable port."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-emacs.cider.direct-eval :as de]
            [nrepl.server :as server]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def ^:private srv (atom nil))

(use-fixtures :once
  (fn [f]
    (reset! srv (server/start-server :bind "127.0.0.1" :port 0))
    (try (f) (finally (server/stop-server @srv)))))

(defn- port [] (:port @srv))

(deftest value-and-output
  (is (= {:ok "3"} (select-keys (de/eval-on-port (port) "(+ 1 2)") [:ok])))
  (is (= "hi\n:k" (:ok (de/eval-on-port (port) "(println \"hi\") :k"))))
  (testing "multi-form code answers with the last value"
    (is (= "6" (:ok (de/eval-on-port (port) "(def x 5) (inc x)"))))))

(deftest an-eval-longer-than-five-seconds-returns
  (is (= ":slept" (:ok (de/eval-on-port (port) "(Thread/sleep 6000) :slept"
                                        {:timeout-ms 20000})))))

(deftest a-thrown-exception-is-an-error
  (let [r (de/eval-on-port (port) "(throw (ex-info \"boom\" {}))")]
    (is (= :cider/eval-error (:error r)))
    (is (str/includes? (:message r) "boom"))))

(deftest the-deadline-interrupts
  (let [t0 (System/currentTimeMillis)
        r  (de/eval-on-port (port) "(Thread/sleep 30000)" {:timeout-ms 1000})]
    (is (= :cider/eval-timeout (:error r)))
    (is (:timed-out r))
    (is (< (- (System/currentTimeMillis) t0) 10000))))

(deftest nothing-on-the-port-is-unreachable
  (let [free (with-open [s (java.net.ServerSocket. 0)] (.getLocalPort s))]
    (is (= :cider/port-unreachable (:error (de/eval-on-port free "1"))))))
