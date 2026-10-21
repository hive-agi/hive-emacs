(ns hive-emacs.daemon-liveness-test
  "Liveness-first heartbeat against recording ping ports, a stub liveness
   probe and an injected clock: emacsclient runs only when health drops."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-emacs.daemon :as daemon]
            [hive-emacs.daemon-ds :as daemon-ds]
            [hive-emacs.daemon-liveness :as liveness]
            [hive-emacs.daemon-store :as daemon-store]))

(def ^:private t0 1789309000000)

(defn- clock
  "A settable clock: (now-fn) -> Date of @ms."
  [ms]
  (let [state (atom ms)]
    {:ms state
     :now-fn #(java.util.Date. (long @state))}))

(defn- recording-ping
  "Ping port that records every daemon it was asked about."
  [calls answer]
  (fn [daemon-id]
    (swap! calls conj daemon-id)
    answer))

(defn- store-with
  [clk & ids]
  (let [store (daemon-ds/create-store {:now-fn (:now-fn clk)})]
    (doseq [id ids] (daemon/register! store id {:socket-name id}))
    store))

(def ^:private ok-ping {:success true :result "4242" :duration-ms 12})

(deftest ping-needed-only-when-the-probe-cannot-vouch
  (let [healthy {:emacs-daemon/health-score 100
                 :emacs-daemon/error-count 0
                 :emacs-daemon/pinged-at (java.util.Date. t0)}]
    (is (false? (liveness/ping-needed? healthy :alive (+ t0 1000))))
    (testing "health drops"
      (is (liveness/ping-needed? healthy :dead (+ t0 1000)))
      (is (liveness/ping-needed? healthy :unknown (+ t0 1000)))
      (is (liveness/ping-needed? (assoc healthy :emacs-daemon/error-count 1) :alive (+ t0 1000)))
      (is (liveness/ping-needed? (assoc healthy :emacs-daemon/health-score 50) :alive (+ t0 1000))))
    (testing "never pinged, or the last real ping is too old"
      (is (liveness/ping-needed? (dissoc healthy :emacs-daemon/pinged-at) :alive t0))
      (is (liveness/ping-needed? healthy :alive (+ t0 liveness/confirm-interval-ms))))))

(deftest learned-pid-reads-the-emacs-pid-answer
  (is (= 4242 (liveness/learned-pid {:result "4242"})))
  (is (= 4242 (liveness/learned-pid {:result " 4242\n"})))
  (is (= 7 (liveness/learned-pid {:result 7})))
  (is (nil? (liveness/learned-pid {:result "t"})))
  (is (nil? (liveness/learned-pid {}))))

(deftest pid-liveness-probes-the-process-table
  (is (= :unknown (liveness/pid-liveness {})))
  (is (= :alive (liveness/pid-liveness
                 {:emacs-daemon/pid (.pid (java.lang.ProcessHandle/current))}))))

(deftest first-heartbeat-pings-and-learns-the-pid-then-probes-locally
  (let [clk (clock t0)
        store (store-with clk "server")
        calls (atom [])
        probed (atom [])
        opts {:ping-fn (recording-ping calls ok-ping)
              :liveness-fn (fn [d] (swap! probed conj (:emacs-daemon/pid d))
                             (if (:emacs-daemon/pid d) :alive :unknown))
              :now-fn (:now-fn clk)}]
    (testing "no pid known yet: one real ping, which answers the pid"
      (is (= :ping (:probe (liveness/heartbeat! store "server" opts))))
      (is (= ["server"] @calls))
      (is (= 4242 (:emacs-daemon/pid (daemon/get-daemon store "server")))))
    (testing "30 s later the live pid vouches: no emacsclient"
      (reset! (:ms clk) (+ t0 30000))
      (let [report (liveness/heartbeat! store "server" opts)]
        (is (= :liveness (:probe report)))
        (is (:healthy? report))
        (is (= 1 (count @calls)))
        (is (= (java.util.Date. (+ t0 30000))
               (:emacs-daemon/heartbeat-at (daemon/get-daemon store "server")))
            "the vouched heartbeat keeps the daemon out of stale cleanup")))
    (testing "after the confirm interval a real ping runs again"
      (reset! (:ms clk) (+ t0 liveness/confirm-interval-ms))
      (is (= :ping (:probe (liveness/heartbeat! store "server" opts))))
      (is (= 2 (count @calls))))))

(deftest a-dead-pid-falls-back-to-emacsclient-and-scores-the-failure
  (let [clk (clock t0)
        store (store-with clk "server")
        calls (atom [])
        opts {:ping-fn (recording-ping calls {:success false :error "down"})
              :liveness-fn (constantly :dead)
              :now-fn (:now-fn clk)}]
    (daemon/update-daemon! store "server" {:emacs-daemon/pid 4242
                                           :emacs-daemon/pinged-at (java.util.Date. t0)})
    (let [report (liveness/heartbeat! store "server" opts)]
      (is (= :ping (:probe report)))
      (is (false? (:success? report)))
      (is (= ["server"] @calls))
      (is (= 1 (:emacs-daemon/error-count (daemon/get-daemon store "server")))))))

(deftest a-throwing-probe-is-treated-as-unknown
  (let [clk (clock t0)
        store (store-with clk "server")
        calls (atom [])]
    (liveness/heartbeat! store "server"
                         {:ping-fn (recording-ping calls ok-ping)
                          :liveness-fn (fn [_] (throw (ex-info "no procfs" {})))
                          :now-fn (:now-fn clk)})
    (is (= ["server"] @calls))))

(deftest heartbeat-tick-pings-only-daemons-whose-health-dropped
  (let [clk (clock t0)
        store (store-with clk "a" "b" "c")
        calls (atom [])
        opts {:ping-fn (recording-ping calls ok-ping)
              :liveness-fn (fn [d] (if (= "b" (:emacs-daemon/id d)) :dead :alive))
              :now-fn (:now-fn clk)}]
    (testing "cold start: every daemon is pinged once"
      (daemon-store/heartbeat-tick! store opts t0 t0)
      (is (= #{"a" "b" "c"} (set @calls))))
    (testing "steady state: only the daemon whose pid is gone is pinged"
      (reset! calls [])
      (reset! (:ms clk) (+ t0 30000))
      (let [cleanup-at (daemon-store/heartbeat-tick! store opts (+ t0 30000) t0)]
        (is (= ["b"] @calls))
        (is (= t0 cleanup-at) "maintenance not due yet")
        (is (= 1 (:last-pings (daemon-store/heartbeat-status))))
        (is (= 3 (:last-probes (daemon-store/heartbeat-status))))))
    (testing "maintenance runs once its interval passed and moves the mark"
      (let [later (+ t0 (* 2 60 1000))]
        (reset! (:ms clk) later)
        (is (= later (daemon-store/heartbeat-tick! store opts later t0)))))))
