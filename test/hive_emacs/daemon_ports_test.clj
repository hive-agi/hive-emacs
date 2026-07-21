(ns hive-emacs.daemon-ports-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [hive-emacs.daemon-ports :as ports]))

(defn clear-ports-fixture [f]
  (ports/clear!)
  (try
    (f)
    (finally
      (ports/clear!))))

(use-fixtures :each clear-ports-fixture)

(deftest unconfigured-ping-is-explicit
  (is (= {:success false
          :success? false
          :error :emacs/port-unavailable
          :port :ping-fn
          :port-unconfigured true}
         (ports/ping! "server"))))

(deftest configured-ports-delegate
  (let [events (atom [])]
    (ports/configure!
     {:ping-fn (fn [daemon-id]
                 {:success true :daemon-id daemon-id})
      :emit-fn (fn [event payload]
                 (swap! events conj [event payload]))})
    (is (= {:success true :daemon-id "server"}
           (ports/ping! "server")))
    (ports/emit! :daemon/test {:ok true})
    (is (= [[:daemon/test {:ok true}]] @events))))

(deftest emitter-failure-does-not-escape
  (ports/configure! {:emit-fn (fn [_ _]
                                (throw (ex-info "boom" {})))})
  (is (nil? (ports/emit! :daemon/test {}))))
