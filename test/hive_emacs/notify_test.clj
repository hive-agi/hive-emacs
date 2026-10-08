(ns hive-emacs.notify-test
  "Back-compat notification shim tested against the INotify port, without a desktop."
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [hive-emacs.notify :as legacy]
            [hive-spi.notify :as notify]
            [hive-test.trifecta :refer [deftrifecta]]))

(defn- observe-notification
  [{:keys [options delivered?]}]
  (let [calls (atom [])
        backend (reify notify/INotify
                  (notify-id [_] :desktop)
                  (backend-available? [_] true)
                  (accepts? [_ _] true)
                  (notify! [_ notification]
                    (swap! calls conj notification)
                    {:delivered? delivered? :backend :desktop}))]
    (binding [legacy/*desktop-backend*
              (fn [config]
                (swap! calls conj config)
                backend)]
      {:result (legacy/notify! options)
       :calls @calls})))

(deftrifecta legacy-notify-adapter
  observe-notification
  {:golden-path "test/golden/hive_emacs/notify_adapter.edn"
   :cases {:default {:options {:summary "Ready" :body "Hello"} :delivered? true}
           :warning {:options {:summary "Careful" :body "Disk" :type "warning"
                               :app-name "custom" :timeout 9000} :delivered? true}
           :error-failed {:options {:summary "Oops" :type "error"} :delivered? false}
           :unknown-type {:options {:summary "Other" :type "other"} :delivered? true}}
   :gen (gen/return {:options {:summary "Ready" :body "Hello"} :delivered? true})
   :pred (fn [{:keys [result calls]}]
           (and (boolean? result)
                (= 2 (count calls))
                (= :info (:level (second calls)))))
   :num-tests 20
   :mutations [["always-false" (fn [_] {:result false :calls []})]
               ["wrong-level" (fn [_] {:result true :calls [{:app "hive-mcp"}
                                                              {:summary "Ready" :body "Hello"
                                                               :level :error}]})]]})

(deftest backend-exception-degrades-to-false
  (binding [legacy/*desktop-backend* (fn [_] (throw (ex-info "desktop offline" {})))]
    (is (false? (legacy/notify! {:summary "Offline"})))))
