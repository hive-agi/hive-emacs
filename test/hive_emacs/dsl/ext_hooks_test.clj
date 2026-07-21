(ns hive-emacs.dsl.ext-hooks-test
  (:require [clojure.test :refer [deftest is testing]]
            [hive-emacs.bridge-loader :as bridge]
            [hive-emacs.client :as ec]
            [hive-emacs.dsl.ext-hooks :as hooks]))

(deftest contributions-expose-bridge-aware-evaluators
  (testing "stable extension keys point at retryable bridge evaluators"
    (is (identical? #'hooks/eval-elisp
                    (:emacs/eval-elisp hooks/contributions)))
    (is (identical? #'hooks/eval-elisp-with-timeout
                    (:emacs/eval-elisp-with-timeout hooks/contributions)))))

(deftest evaluator-recovers-when-bridge-becomes-ready
  (let [ready? (atom false)
        calls (atom [])]
    (with-redefs [bridge/ensure-loaded! (fn [_] @ready?)
                  ec/eval-elisp-with-timeout
                  (fn [code timeout]
                    (swap! calls conj [code timeout])
                    {:success true :result code})]
      (is (:bridge-unavailable
           (hooks/eval-elisp-with-timeout "first" 1000)))
      (reset! ready? true)
      (is (= {:success true :result "second"}
             (hooks/eval-elisp-with-timeout "second" 2000))))
    (is (= [["second" 2000]] @calls))))
