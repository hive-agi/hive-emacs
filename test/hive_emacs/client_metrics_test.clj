(ns hive-emacs.client-metrics-test
  "emacsclient runs are recorded as `[:proc/shell \"emacsclient\"]`; recording
   is total and never changes the eval answer."
  (:require [clojure.test :refer [deftest is]]
            [hive-emacs.client :as client]))

(deftest record-proc-is-total
  (is (nil? (client/record-proc! (System/nanoTime) :ok)))
  (is (nil? (client/record-proc! (System/nanoTime) :error))))

(deftest eval-through-a-stub-transport-is-unchanged
  (client/reset-circuit-breaker!)
  (let [prev (client/set-transport! (fn [_argv] {:exit 0 :out "\"hi\"" :err ""}))]
    (try
      (is (= {:success true :result "hi"}
             (select-keys (client/eval-elisp "t") [:success :result])))
      (finally (client/set-transport! prev)
               (client/reset-circuit-breaker!)))))
