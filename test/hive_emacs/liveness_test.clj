(ns hive-emacs.liveness-test
  "The Emacs liveness breaker, driven through an injected probe port and an
   injected clock. No with-redefs, and no test reaches a live Emacs: the
   client's transport is a recording stub that never runs emacsclient."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-dsl.result :as result]
            [hive-emacs.client :as client]
            [hive-emacs.liveness :as liveness]
            [hive-emacs.tools.cider :as cider]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def ^:private t0 1795400000000)

(def ^:private good {:ok? true :pid 49094 :gc-elapsed 100.0 :gcs-done 10})

(def ^:private hung {:ok? false :timed-out? true
                     :error "Server not responding; timed out after 3 seconds"})

(defn- scripted-probe
  "A probe port answering from the atom ANSWER, counting its calls."
  [answer calls]
  (fn [] (swap! calls inc) @answer))

(defn- refuse-transport
  [calls]
  (fn [argv] (swap! calls conj argv) {:exit 0 :out "t" :err ""}))

(use-fixtures :each
  (fn [f]
    (let [previous (client/set-transport! (fn [argv]
                                            (throw (ex-info "test reached a live emacsclient"
                                                            {:argv argv}))))]
      (client/reset-circuit-breaker!)
      (try (f)
           (finally
             (liveness/uninstall!)
             (client/set-transport! previous)
             (client/shutdown-executor!)
             (client/reset-circuit-breaker!))))))

;;; ---------------------------------------------------------------------------
;;; Pure state machine
;;; ---------------------------------------------------------------------------

(deftest step-opens-on-a-failed-probe-and-closes-on-a-good-one
  (let [s0 (liveness/initial-state)
        {s1 :state t1 :transition} (liveness/step s0 good t0)
        {s2 :state t2 :transition} (liveness/step s1 hung (+ t0 10000))
        {s3 :state t3 :transition} (liveness/step s2 hung (+ t0 20000))
        {s4 :state t4 :transition} (liveness/step s3 good (+ t0 30000))]
    (is (= [:closed :open :open :closed] (map :state [s1 s2 s3 s4])))
    (is (= [nil :opened nil :closed] [t1 t2 t3 t4])
        "a transition is reported once each way, never per probe")
    (is (= :busy (:cause s2)))
    (is (= 2 (:failures s3)))
    (is (= (+ t0 10000) (:opened-at s3)) "opened-at keeps the first failure")))

(deftest gc-growth-names-gc-thrash-not-busy
  (let [s1 (:state (liveness/step (liveness/initial-state) good t0))
        ;; 9.5 s of GC in 10 s of wall time: the 2026-11-20 incident.
        s2 (:state (liveness/step s1 (assoc good :gc-elapsed 109.5 :gcs-done 40) (+ t0 10000)))
        s3 (:state (liveness/step s2 hung (+ t0 20000)))]
    (is (< 0.94 (:gc-fraction s2) 0.96))
    (is (:gc-thrash? (liveness/verdict s2 (+ t0 10000))))
    (is (= :gc-thrash (:cause s3)))
    (is (str/includes? (liveness/refusal-message s3 (+ t0 25000)) "GC thrash")))
  (testing "a calm Emacs that stops answering is busy"
    (let [s1 (:state (liveness/step (liveness/initial-state) good t0))
          s2 (:state (liveness/step s1 (assoc good :gc-elapsed 100.2) (+ t0 10000)))
          s3 (:state (liveness/step s2 hung (+ t0 20000)))]
      (is (= :busy (:cause s3)))))
  (testing "a refused socket is down"
    (is (= :down (:cause (:state (liveness/step (liveness/initial-state)
                                                {:ok? false :error "connection refused"}
                                                t0))))))
  (testing "a new Emacs pid does not compare GC counters across processes"
    (let [s1 (:state (liveness/step (liveness/initial-state) good t0))
          s2 (:state (liveness/step s1 (assoc good :pid 1 :gc-elapsed 900.0) (+ t0 10000)))]
      (is (nil? (:gc-fraction s2))))))

(deftest parse-probe-output-reads-pid-and-gc-counters
  (is (= {:pid 135840 :gc-elapsed 145.160183592 :gcs-done 839}
         (liveness/parse-probe-output "(135840 145.160183592 839)\n")))
  (is (nil? (liveness/parse-probe-output "t"))))

(deftest refusal-names-pid-seconds-and-fallback
  (let [s (-> (liveness/initial-state)
              (liveness/step good t0) :state
              (liveness/step hung (+ t0 10000)) :state)
        msg (liveness/refusal-message s (+ t0 42000))]
    (is (str/includes? msg "pid 49094"))
    (is (str/includes? msg "42 s since the last good probe"))
    (is (str/includes? msg "Fallback"))
    (is (str/includes? msg "port"))))

;;; ---------------------------------------------------------------------------
;;; Gate, through the injected probe port
;;; ---------------------------------------------------------------------------

(deftest gate-caches-the-probe-and-refuses-at-once-while-open
  (let [answer (atom good)
        probes (atom 0)
        now (atom t0)
        transitions (atom [])
        sent (atom [])]
    (client/set-transport! (refuse-transport sent))
    (liveness/install! {:probe-fn (scripted-probe answer probes)
                        :now-fn #(deref now)
                        :on-transition (fn [t _] (swap! transitions conj t))})
    (testing "closed: calls proceed, one probe per 10 s window"
      (is (:success (client/eval-elisp-with-timeout "(+ 1 1)" 1000)))
      (is (:success (client/eval-elisp-with-timeout "(+ 1 1)" 1000)))
      (is (= 1 @probes))
      (is (= 2 (count @sent))))
    (testing "the hang is seen on the first call after the cache expires"
      (reset! answer hung)
      (swap! now + 10000)
      (let [start (System/nanoTime)
            r (client/eval-elisp-with-timeout "(+ 1 1)" 30000)
            ms (/ (- (System/nanoTime) start) 1e6)]
        (is (false? (:success r)))
        (is (:emacs-unresponsive r))
        (is (< ms 1000) "refused at once, not after the call's own timeout")
        (is (str/includes? (:error r) "pid 49094"))
        (is (= 2 (count @sent)) "no emacsclient ran for the refused call")))
    (testing "while open, cached: no probe, no emacsclient, every call refused"
      (dotimes [_ 5] (is (:emacs-unresponsive (client/eval-elisp-with-timeout "t" 1000))))
      (is (= 2 @probes))
      (is (= 2 (count @sent))))
    (testing "recovery closes the breaker within one cache window"
      (reset! answer (assoc good :gc-elapsed 101.0))
      (swap! now + 10000)
      (is (:success (client/eval-elisp-with-timeout "t" 1000)))
      (is (= :closed (:state (liveness/status)))))
    (is (= [:opened :closed] @transitions))))

(deftest a-throwing-probe-counts-as-a-failed-probe
  (liveness/install! {:probe-fn #(throw (ex-info "boom" {}))
                      :now-fn (constantly t0)
                      :on-transition (fn [_ _])})
  (let [r (client/eval-elisp-with-timeout "t" 1000)]
    (is (:emacs-unresponsive r))
    (is (str/includes? (:error r) "probe threw: boom"))))

(deftest an-uninstalled-gate-admits-everything
  (let [sent (atom [])]
    (client/set-transport! (refuse-transport sent))
    (is (nil? (liveness/admit)))
    (is (:success (client/eval-elisp-with-timeout "t" 1000)))
    (is (= 1 (count @sent)))))

;;; ---------------------------------------------------------------------------
;;; The production probe, over a stub transport
;;; ---------------------------------------------------------------------------

(deftest probe-emacs-uses-emacsclient-own-timeout
  (let [argvs (atom [])]
    (client/set-transport! (fn [argv] (swap! argvs conj argv)
                             {:exit 0 :out "(49094 12.5 7)\n" :err ""}))
    (is (= {:ok? true :pid 49094 :gc-elapsed 12.5 :gcs-done 7} (client/probe-emacs!)))
    (let [argv (first @argvs)]
      (is (= ["-w" "3"] (take 2 (drop-while #(not= "-w" %) argv))))
      (is (= client/probe-elisp (last argv)))))
  (testing "emacsclient's 'not responding' is a timed-out probe"
    (client/set-transport! (constantly {:exit 1 :out ""
                                        :err "Server not responding; timed out after 3 seconds\n"}))
    (is (= {:ok? false :timed-out? true
            :error "Server not responding; timed out after 3 seconds"}
           (client/probe-emacs!)))))

;;; ---------------------------------------------------------------------------
;;; cider verbs behind the open breaker
;;; ---------------------------------------------------------------------------

(defn- open-breaker!
  []
  (liveness/install! {:probe-fn (constantly hung)
                      :now-fn (constantly t0)
                      :on-transition (fn [_ _])})
  (liveness/check!))

(deftest cider-spawn-refuses-at-once-and-names-the-hang
  (open-breaker!)
  (let [start (System/nanoTime)
        r (cider/handle-spawn {:name "x" :project_dir (System/getProperty "user.dir")})
        ms (/ (- (System/nanoTime) start) 1e6)
        text (str (:text r) (get-in r [:content 0 :text]) (:error r) (pr-str r))]
    (is (< ms 5000))
    (is (str/includes? text "Emacs is not answering"))))

(deftest an-eval-that-names-a-port-is-not-refused
  (open-breaker!)
  (let [seen (atom nil)]
    (binding [cider/*direct-eval-fn* (fn [port code _opts]
                                       (reset! seen [port code])
                                       (result/ok "2"))]
      (let [r (cider/handle-eval {:code "(+ 1 1)" :port 7999})]
        (is (= [7999 "(+ 1 1)"] @seen))
        (is (not (str/includes? (pr-str r) "Emacs is not answering")))))))

(deftest cider-liveness-verb-reports-the-verdict
  (open-breaker!)
  (let [r (cider/handle-liveness {})
        body (json/read-str (or (:text r) (get-in r [:content 0 :text])) :key-fn keyword)]
    (is (= "open" (:state body)))
    (is (= "busy" (:cause body)))))
