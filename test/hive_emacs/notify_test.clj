(ns hive-emacs.notify-test
  "Back-compat notification shim tested against the INotify port, without a desktop."
  (:require [clojure.test :refer [deftest is testing]]
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

(defn- observe-frame-notification
  [{:keys [notification success?]}]
  (let [codes (atom [])]
    (binding [legacy/*eval-fn* (fn [code]
                                 (swap! codes conj code)
                                 {:success success? :result "t" :error (when-not success? "no emacs")})]
      {:result (try (legacy/frame-notify! notification)
                    (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))
       :codes @codes})))

(deftrifecta frame-notify-adapter
  observe-frame-notification
  {:golden-path "test/golden/hive_emacs/frame_notify_adapter.edn"
   :cases {:info {:notification {:summary "Ready" :body "Hello" :level :info} :success? true}
           :error-no-body {:notification {:summary "Boom" :level :error} :success? true}
           :quoting {:notification {:summary "Say \"hi\"" :body "50% done" :level :warn} :success? true}
           :emacs-down {:notification {:summary "Down"} :success? false}}
   :gen (gen/return {:notification {:summary "Ready" :body "Hello" :level :info} :success? true})
   :pred (fn [{:keys [result codes]}]
           (and (true? result) (= 1 (count codes))))
   :num-tests 20
   :mutations [["never-calls-emacs" (fn [_] {:result true :codes []})]
               ["swallows-failure" (fn [_] {:result true :codes ["x" "y"]})]]})

(deftest widget-backend-routes-through-frame-notify
  (testing "WidgetBackend built here resolves frame-notify! as its bridge"
    (let [codes (atom [])
          backend (legacy/widget-backend)]
      (binding [legacy/*eval-fn* (fn [code] (swap! codes conj code) {:success true :result "t"})]
        (is (true? (notify/backend-available? backend)))
        (is (true? (:delivered? (notify/notify! backend {:summary "Done" :level :info}))))
        (is (= 1 (count @codes))))
      (binding [legacy/*eval-fn* (fn [_] {:success false :error "no emacs"})]
        (let [r (notify/notify! backend {:summary "Done" :level :info})]
          (is (false? (:delivered? r)))
          (is (= :bridge-threw (get-in r [:detail :reason]))))))))
