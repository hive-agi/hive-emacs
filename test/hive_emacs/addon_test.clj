(ns hive-emacs.addon-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is use-fixtures]]
            [hive-addon.mount :as mount]
            [hive-addon.protocol :as addon]
            [hive-emacs.addon :as emacs-addon]
            [hive-emacs.bridge-loader :as bridge]
            [hive-emacs.client :as client]
            [hive-emacs.daemon-store :as daemon-store]
            [hive-emacs.runtime-ports :as ports]
            [hive-emacs.test-support :as support]
            [hive-test.isolation :as isolation]))

(def ^:private world (atom (support/empty-world)))

(use-fixtures :each
  (isolation/with-isolations
   {:type :hive-emacs/runtime}))

(defn- ports-clear?
  []
  (every? nil? (vals (ports/snapshot))))

(deftest constructor-implements-leaf-addon-contract
  (let [instance (emacs-addon/addon-ctor {:source :test})]
    (is (addon/addon? instance))
    (is (= "hive.emacs" (addon/addon-id instance)))
    (is (= :native (addon/addon-type instance)))
    (is (= #{:tools :mcp-bridge :health-reporting :editor :vessel :terminal}
           (addon/capabilities instance)))
    (is (= [] (addon/tools instance)))
    (is (= [] (addon/schema-extensions instance)))
    (is (= {} (addon/hooks instance)))
    (is (= :down (:status (addon/health instance))))
    (is (ports-clear?))))

(deftest manifest-points-at-canonical-constructor
  (let [manifest (some-> "META-INF/hive-addons/hive-emacs.edn"
                         io/resource slurp edn/read-string)]
    (is (= "hive.emacs" (:addon/id manifest)))
    (is (= "hive-emacs.addon" (:addon/init-ns manifest)))
    (is (= "addon-ctor" (:addon/init-fn manifest)))
    (is (= #{"hive-mcp" "hive-mcp-api" "hive-mcp-cider"
             "hive-mcp-cider-eval" "hive-mcp-cider-sessions"}
           (get-in manifest [:addon/doctor :emacs/features])))))

(deftest hive-addon-discovers-manifest-from-classpath
  (let [{:keys [specs errors]} (mount/discover-specs)
        spec (first (filter #(= "hive.emacs" (:addon/id %)) specs))]
    (is (empty? errors))
    (is (some? spec))
    (is (= "hive-emacs.addon" (:addon/init-ns spec)))
    (is (= "addon-ctor" (:addon/init-fn spec)))))

(deftest unavailable-bridge-does-not-block-activation
  (with-redefs [bridge/ensure-loaded! (constantly false)
                client/emacs-running? (constantly true)]
    (let [instance (emacs-addon/make-addon)
          initialized (addon/initialize! instance {})]
      (is (:success? initialized))
      (is (= :emacsclient (get-in initialized [:metadata :editor-id])))
      (is (false? (get-in initialized [:metadata :bridge-ready?])))
      (is (= :ok (:status (addon/health instance))))
      (is (seq (addon/tools instance)))
      (is (contains? (addon/hooks instance) :emacs/editor))
      (is (contains? (addon/hooks instance) :emacs/vessel))
      (is (nil? (addon/shutdown! instance)))
      (is (ports-clear?)))))

(deftest lifecycle-owns-ports-and-is-idempotent
  (let [ping-fn (fn [daemon-id]
                  {:success true :duration-ms 1 :daemon-id daemon-id})
        emit-fn (fn [event payload]
                  (swap! world update :events conj [event payload]))
        instance (emacs-addon/addon-ctor
                  {:addon/config {:emacs/ping-fn ping-fn
                                  :emacs/event-emitter emit-fn}})]
    (with-redefs [bridge/ensure-loaded! (constantly true)]
      (let [initialized (addon/initialize! instance {})
            repeated (addon/initialize! instance {})]
        (is (:success? initialized))
        (is (:already-initialized? repeated))
        (is (identical? ping-fn (:ping-fn (ports/snapshot))))
        (is (identical? emit-fn (:emit-fn (ports/snapshot))))
        (is (= "server" (:daemon-id (ports/ping! "server"))))
        (is (some? (daemon-store/get-daemon
                    (daemon-store/default-daemon-id))))
        (is (nil? (addon/shutdown! instance)))
        (is (ports-clear?))
        (is (nil? (addon/shutdown! instance)))))))

(deftest failed-initialization-rolls-back-ports
  (with-redefs [bridge/ensure-loaded! (constantly true)]
    (let [instance (emacs-addon/make-addon
                    {:emacs/ports {:not-a-port identity}})
          result (addon/initialize! instance {})]
      (is (false? (:success? result)))
      (is (re-find #"Unknown hive-emacs runtime ports"
                   (first (:errors result))))
      (is (ports-clear?))
      (is (= :down (:status (addon/health instance)))))))

(deftest legacy-entrypoint-constructs-without-initializing
  (let [instance (emacs-addon/init-as-addon!)]
    (is (addon/addon? instance))
    (is (= :down (:status (addon/health instance))))))
