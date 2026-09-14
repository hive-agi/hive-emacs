(ns hive-emacs.vessel-test
  "The Emacs hive-vessel target against a recording evaluator (the port the
   bridge-aware evaluator fills in production): every native reaches the
   evaluator in order with the configured timeout, a failure stops the batch
   loudly, and the Elisp the standard sample batch produces is golden-locked
   so a hive-vessel bump that changes a lowering is a visible diff here."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-addon.protocol :as addon]
            [hive-emacs.addon :as emacs-addon]
            [hive-emacs.vessel :as vessel]
            [hive-test.golden :refer [deftest-golden]]
            [hive-vessel.core :as v]
            [hive-vessel.doc :as d]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defn- recording-evaluator
  "[eval-fn log]: answers {:success true :result n} for the n-th call, or the
   failure FAIL-WITH from the call numbered FAIL-AT on."
  ([] (recording-evaluator nil nil))
  ([fail-at fail-with]
   (let [log (atom [])]
     [(fn [code timeout-ms]
        (swap! log conj [code timeout-ms])
        (if (and fail-at (>= (count @log) fail-at))
          (merge {:success false} fail-with)
          {:success true :result (count @log)}))
      log])))

(def sample-doc
  (d/doc "Carto \"Flow\" \\ #3"
         (d/heading "apply write-form")
         (d/para "succeeded" :success)
         (d/fields [["paths" "src/a.clj\nsrc/b.clj"] ["verify" "ok"]])
         (d/items ["one" "two\nmore"])
         (d/code "(defn f [] \"x\")" "clojure")
         (d/diff "@@ -1,2 +1,2 @@\n-(old)\n+(new ü)\n context")
         (d/link "open a" "/tmp/a.clj" 2)))

(def sample-batch
  "The same sample ops hive-vessel.parity-test locks, in one batch."
  [{:op :ui/show-panel :panel/id "olympus/tab-2" :doc sample-doc}
   {:op :ui/notify :message "frame 7 applied"}
   {:op :ui/notify :message "boom\nsecond line" :level :error}
   {:op :ui/open-file :file "/tmp/a b.clj" :line 3 :column 7}
   {:op :ui/open-file :file "/tmp/a.clj"}
   {:op :ui/send-to-terminal :terminal "*vterm*" :text "ls -la\n"}
   {:op :ui/close-panel :panel/id "olympus/tab-2"}])

(deftest the-target-is-an-elisp-vessel-executing-through-the-evaluator
  (let [[eval-fn log] (recording-evaluator)
        target (vessel/target {:eval-fn eval-fn :timeout-ms 1234 :features #{:demo/el}})
        r (v/dispatch! (v/standard-registry) target sample-batch)]
    (is (= {:vessel/id :emacs :vessel/dialect :elisp :vessel/features #{:demo/el}}
           (dissoc target :vessel/execute!)))
    (is (:ok r) (pr-str (:error r)))
    (is (= (range 1 8) (get-in r [:ok :plan/results])) "each result is what the evaluator answered")
    (is (= (mapv :native/payload (get-in r [:ok :plan/ops])) (mapv first @log))
        "every native's Elisp reaches the evaluator, in order")
    (is (every? #(= 1234 (second %)) @log) "with the configured timeout")))

(deftest an-evaluator-failure-stops-the-batch-loudly
  (doseq [[label failure] {:error {:error "void-function foo"}
                           :timeout {:error "Emacsclient call timed out after 10ms" :timed-out true}
                           :circuit-open {:error "Circuit breaker open" :circuit-open true}}]
    (let [[eval-fn _] (recording-evaluator 3 failure)
          r (v/dispatch! (v/standard-registry) (vessel/target {:eval-fn eval-fn}) sample-batch)
          err (:error r)]
      (testing (name label)
        (is (= :execute-threw (:failure/reason err)))
        (is (= 2 (get-in err [:failure/detail :completed])))
        (is (.contains ^String (get-in err [:failure/detail :message]) (:error failure)))))))

(deftest only-elisp-natives-are-accepted
  (is (thrown? clojure.lang.ExceptionInfo
               ((:vessel/execute! (vessel/target {:eval-fn (fn [_ _] {:success true})}))
                {:op :vessel/native :native/dialect :json :native/payload {}}))))

(deftest the-addon-exposes-the-target-through-the-hook-only-while-active
  (let [a (emacs-addon/make-addon)]
    (is (nil? (get (addon/hooks a) vessel/target-hook-key))
        "an addon that is not active contributes no hooks")))

(deftest-golden the-sample-batch-reaches-emacs-as-this-elisp
  "test/golden/vessel/sample-batch-elisp.edn"
  (let [[eval-fn log] (recording-evaluator)]
    (v/dispatch! (v/standard-registry) (vessel/target {:eval-fn eval-fn}) sample-batch)
    (mapv first @log)))