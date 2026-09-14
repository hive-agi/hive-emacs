(ns hive-emacs.addon-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is use-fixtures]]
            [hive-addon.mount :as mount]
            [hive-addon.protocol :as addon]
            [hive-emacs.addon :as emacs-addon]
            [hive-emacs.attention :as attention]
            [hive-emacs.bridge-loader :as bridge]
            [hive-emacs.client :as client]
            [hive-emacs.daemon-store :as daemon-store]
            [hive-emacs.runtime-ports :as ports]
            [hive-emacs.test-support :as support]
            [hive-test.isolation :as isolation]
            [hive-spi.vessel :as render-port]
            [hive-vessel.renderer :as renderer]
            [hive-emacs.cider.spawn :as spawn]
            [clojure.string :as str]))

(def ^:private world (atom (support/empty-world)))

(use-fixtures :each
  (isolation/with-isolations
   {:type :hive-emacs/runtime}))

(defn- ports-clear?
  []
  (every? nil? (vals (ports/snapshot))))

(defn- recording-eval-fn
  "An elisp boundary that records the code handed to it and never reaches a
   live Emacs. Injected as `:emacs/eval-fn`, so a test cannot touch the
   developer's daemon by forgetting to stub something."
  [calls]
  (fn
    ([code] (swap! calls conj code) {:success true :result "t"})
    ([code _timeout-ms] (swap! calls conj code) {:success true :result "t"})))

(defn- calls-matching
  [calls marker]
  (count (filter #(str/includes? % marker) @calls)))

(def ^:private attention-enable "hive-mcp-attention-enable")

(def ^:private spawn-enable "hive-mcp-cider-spawnwatch-enable")

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
        calls (atom [])
        instance (emacs-addon/addon-ctor
                  {:addon/config {:emacs/ping-fn ping-fn
                                  :emacs/event-emitter emit-fn
                                  :emacs/eval-fn (recording-eval-fn calls)}})]
    (with-redefs [bridge/ensure-loaded! (constantly true)]
      (let [initialized (addon/initialize! instance {})
            repeated (addon/initialize! instance {})]
        (is (:success? initialized))
        (is (satisfies? render-port/IRenderer instance))
        (is (identical? instance (get @renderer/renderers "hive.emacs")))
        (is (:error (render-port/render! instance [{:op :ui/send-to-terminal :text "forbidden"}])))
        (is (:already-initialized? repeated))
        (is (identical? ping-fn (:ping-fn (ports/snapshot))))
        (is (identical? emit-fn (:emit-fn (ports/snapshot))))
        (is (= "server" (:daemon-id (ports/ping! "server"))))
        (is (some? (daemon-store/get-daemon
                    (daemon-store/default-daemon-id))))
        (is (seq @calls)
            "the Emacs halves went through the injected boundary, not emacsclient")
        (is (nil? (addon/shutdown! instance)))
        (is (ports-clear?))
        (is (not (contains? @renderer/renderers "hive.emacs")))
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

(deftest cider-subtree-is-contributed-even-when-the-bridge-is-unavailable
  (let [contributions (atom [])
        instance (emacs-addon/make-addon
                  {:runtime/ports
                   {:extension/contribute-commands!
                    (fn [tool addon-id commands]
                      (swap! contributions conj
                             [tool addon-id (set (keys commands))]))}})]
    (with-redefs [bridge/ensure-loaded! (constantly false)
                  client/emacs-running? (constantly true)]
      (let [initialized (addon/initialize! instance {})]
        (is (:success? initialized))
        (is (false? (get-in initialized [:metadata :bridge-ready?])))
        (is (= [["code" "hive.emacs" #{"cider"}]
                ["emacs" "hive.emacs" #{"attention" "answer"}]]
               @contributions)
            "attention verbs reach the host's `emacs` root as a contribution")
        (is (nil? (addon/shutdown! instance)))
        (is (ports-clear?))))))

(deftest attention-block-is-registered-and-retracted-through-the-host-port
  (let [registered (atom {})
        calls (atom [])
        instance (emacs-addon/make-addon
                  {:emacs/eval-fn (recording-eval-fn calls)
                   :runtime/ports
                   {:extension/register!
                    (fn [k v] (swap! registered assoc k v))}})]
    (with-redefs [bridge/ensure-loaded! (constantly true)
                  client/emacs-running? (constantly true)]
      (let [initialized (addon/initialize! instance {})]
        (is (:success? initialized))
        (is (= {:block? true :publishing? true}
               (get-in initialized [:metadata :attention])))
        (is (= 1 (calls-matching calls attention-enable))
            "the Emacs half is started once the bridge is ready, through the injected boundary")
        (let [upkeep-calls (atom 0)]
          (with-redefs [attention/keep-publishing! (fn [_] (swap! upkeep-calls inc) false)
                        attention/emitter (constantly "BODY")]
            (is (= "BODY" ((get @registered :block/emacs-attention) {}))
                "the registered emitter renders the attention block")
            (is (= 1 @upkeep-calls)
                "and keeps the Emacs half publishing on the way")))
        (is (nil? (addon/shutdown! instance)))
        (is (nil? ((get @registered :block/emacs-attention) {}))
            "shutdown leaves an emitter that renders nothing")))))

(deftest spawn-block-is-registered-and-retracted-through-the-host-port
  (let [registered (atom {})
        calls (atom [])
        instance (emacs-addon/make-addon
                  {:emacs/eval-fn (recording-eval-fn calls)
                   :runtime/ports
                   {:extension/register!
                    (fn [k v] (swap! registered assoc k v))}})]
    (with-redefs [bridge/ensure-loaded! (constantly true)
                  client/emacs-running? (constantly true)]
      (let [initialized (addon/initialize! instance {})]
        (is (:success? initialized))
        (is (= {:block? true :publishing? true}
               (get-in initialized [:metadata :cider-spawn])))
        (is (= 1 (calls-matching calls spawn-enable))
            "the publisher is started once the bridge is ready, through the injected boundary")
        (binding [spawn/*root-fn* (constantly (io/file "/nonexistent-spawn-root"))]
          (spawn/reset-watches!)
          (spawn/watch-spawn! (recording-eval-fn calls)
                              "{\"name\":\"pending-one\",\"port\":7920,\"repl-type\":\"clj\"}")
          (let [body ((get @registered :block/cider-spawn) {})]
            (is (re-find #"pending-one" body)
                "the registered emitter reports the spawn the agent asked for")
            (is (re-find #"still starting" body)
                "and says it is not usable yet while no outcome is published")))
        (is (nil? (addon/shutdown! instance)))
        (is (nil? ((get @registered :block/cider-spawn) {}))
            "shutdown leaves an emitter that renders nothing")
        (is (empty? (spawn/watches))
            "and owes no outcome from a previous lifecycle")))))

(deftest neither-emacs-half-is-started-without-the-bridge
  (let [calls (atom [])
        instance (emacs-addon/make-addon {:emacs/eval-fn (recording-eval-fn calls)})]
    (with-redefs [bridge/ensure-loaded! (constantly false)
                  client/emacs-running? (constantly true)]
      (let [initialized (addon/initialize! instance {})]
        (is (= {:block? false :publishing? false}
               (get-in initialized [:metadata :attention])))
        (is (= {:block? false :publishing? false}
               (get-in initialized [:metadata :cider-spawn])))
        (is (zero? (calls-matching calls attention-enable)))
        (is (zero? (calls-matching calls spawn-enable)))
        (addon/shutdown! instance)))))
