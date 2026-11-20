(ns hive-emacs.cider.admission-test
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-emacs.addon :as emacs-addon]
            [hive-emacs.cider.admission :as admission]
            [hive-emacs.runtime-ports :as ports]
            [hive-emacs.tools.cider :as cider]
            [hive-addon.protocol :as addon])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- temp-dir []
  (str (Files/createTempDirectory "cider-admission-test"
                                  (into-array FileAttribute []))))

(defn- spawn-eval-stub [sessions]
  (let [calls (atom [])]
    {:calls calls
     :eval-fn (fn
                ([code] (swap! calls conj code)
                 (if (str/includes? code "hive-mcp-cider-list-sessions")
                   {:success true :result (json/write-str sessions)}
                   {:success true :result "{}"}))
                ([code _] (swap! calls conj code)
                 {:success true :result (json/write-str sessions)}))}))

(deftest meminfo-is-read-through-a-file-port
  (let [file (io/file (temp-dir) "meminfo")]
    (spit file "MemTotal:       32000000 kB\nMemAvailable:    16777216 kB\n")
    (is (= (* 16 admission/gib) (admission/read-mem-available (str file)))))
  (is (nil? (admission/read-mem-available "/not/a/proc/filesystem"))))

(deftest ram-floor-refuses-with-the-callers-reusable-sessions
  (let [available (dec (* 12 admission/gib))
        refusal ((admission/make-gate {:mem-available-fn (constantly available)})
                 {:caller-id "agent-a" :project-dir "/gone"
                  :sessions [{:name "mine" :status "connected" :owner-id "agent-a"
                              :project-dir "/p" :port 54321}
                             {:name "other" :status "connected" :owner-id "agent-b"}
                             {:name "starting" :status "starting" :owner-id "agent-a"}]})]
    (is (= :cider/insufficient-memory (:error refusal)))
    (is (= available (:mem_available_bytes refusal)))
    (is (= (* 12 admission/gib) (:minimum_ram_bytes refusal)))
    (is (= :cider/spawn-min-ram-gb (:config_key refusal)))
    (is (= [{:name "mine" :project-dir "/p" :port 54321 :owner-id "agent-a"}]
           (:connected_sessions refusal)))
    (is (str/includes? (:message refusal) "mine"))
    (is (not (str/includes? (:message refusal) "other")))))

(deftest boundary-admits-and-configured-floor-is-honored
  (is (nil? ((admission/make-gate {:mem-available-fn (constantly (* 12 admission/gib))})
             {})))
  (let [refusal ((admission/make-gate {:min-ram-gb 2
                                       :mem-available-fn (constantly admission/gib)}) {})]
    (is (= 2 (:minimum_ram_gb refusal))))
  (is (thrown? clojure.lang.ExceptionInfo (admission/make-gate {:min-ram-gb -1}))))

(deftest addon-config-installs-the-floor-and-injected-memory-port
  (let [dir (temp-dir)
        {:keys [calls eval-fn]} (spawn-eval-stub [])
        instance (emacs-addon/make-addon
                  {:cider/spawn-min-ram-gb 2
                   :emacs/mem-available-fn (constantly (* 1 admission/gib))
                   :emacs/eval-fn (fn ([_] {:success true :result "t"})
                                    ([_ _] {:success true :result "t"}))
                   :runtime/ports {:extension/contribute-commands!
                                   (fn [_ _ _] nil)}})
        initialized (addon/initialize! instance {})]
    (is (:success? initialized))
    (try
      (let [response (binding [cider/*eval-fn* eval-fn]
                       (cider/handle-spawn {:name "configured-floor" :project_dir dir}))]
        (is (:isError response))
        (is (= 2 (:minimum_ram_gb response)))
        (is (not-any? #(str/includes? % "spawn-session") @calls)))
      (finally (addon/shutdown! instance)))))

(deftest missing-meminfo-admits-and-logs-once
  (let [messages (atom [])
        gate (admission/make-gate {:mem-available-fn (constantly nil)
                                   :log-fn #(swap! messages conj %)})]
    (is (nil? (gate {})))
    (is (nil? (gate {})))
    (is (= 1 (count @messages)))))

(deftest explicit-spawn-never-launches-below-the-floor
  (let [dir (temp-dir)
        {:keys [calls eval-fn]} (spawn-eval-stub
                                 [{:name "mine" :status "connected" :owner-id "caller"
                                   :repl-type "clj" :project-dir dir :port 54321}])
        reaped (atom 0)
        response (binding [admission/*gate-fn*
                           (admission/make-gate {:mem-available-fn
                                                 (constantly admission/gib)})
                           cider/*eval-fn* eval-fn
                           cider/*reap-fn* #(swap! reaped inc)]
                   (cider/handle-spawn {:name "new" :project_dir dir :_caller_id "caller"}))]
    (is (:isError response))
    (is (= :cider/insufficient-memory (:error response)))
    (is (= ["mine"] (mapv :name (:connected_sessions response))))
    (is (= 1 @reaped) "cleanup runs before admission")
    (is (= 1 (count @calls)) "the sole call lists sessions; no spawn reaches Emacs")
    (is (str/includes? (first @calls) "list-sessions"))))

(deftest spawn-reaps-orphans-without-a-host-shout-port
  (let [missing (str (temp-dir) "/deleted-worktree")
        {:keys [calls eval-fn]} (spawn-eval-stub
                                 [{:name "orphan" :status "connected" :owned? true
                                   :repl-type "clj" :project-dir missing}
                                  {:name "external" :status "connected" :owned? false
                                   :repl-type "clj" :project-dir missing}])
        response (binding [admission/*gate-fn*
                           (admission/make-gate {:mem-available-fn
                                                 (constantly admission/gib)})
                           cider/*eval-fn* eval-fn]
                   (cider/handle-spawn {:name "new" :project_dir (temp-dir)}))
        kills (filter #(str/includes? % "kill-session") @calls)]
    (is (= :cider/insufficient-memory (:error response)))
    (is (= 1 (count kills)) "only the owned orphan is stopped")
    (is (str/includes? (first kills) "orphan"))
    (is (not-any? #(str/includes? % "external") kills))))

(deftest auto-connect-spawn-is-refused-before-launch
  (let [dir (temp-dir)
        {:keys [calls eval-fn]} (spawn-eval-stub [])
        response (binding [admission/*gate-fn*
                           (admission/make-gate {:mem-available-fn
                                                 (constantly admission/gib)})
                           cider/*eval-fn* eval-fn
                           cider/*reap-fn* (fn [])]
                   (cider/handle-eval {:code "(+ 1 2)" :project_dir dir}))]
    (is (:isError response))
    (is (= :cider/insufficient-memory (:error response)))
    (is (not-any? #(str/includes? % "spawn-session") @calls))))

(deftest ensure-connected-port-propagates-ram-refusal
  (let [dir (temp-dir)
        {:keys [calls eval-fn]} (spawn-eval-stub
                                 [{:name "caller-owned" :status "connected"
                                   :owner-id "caller" :project-dir "/different"}])]
    (binding [admission/*gate-fn*
              (admission/make-gate {:mem-available-fn (constantly admission/gib)})
              cider/*eval-fn* eval-fn cider/*reap-fn* (fn [])]
      (let [error (try (cider/ensure-connected dir)
                       (catch clojure.lang.ExceptionInfo e (ex-data e)))]
        (is (= :cider/insufficient-memory (:error error)))
        (is (not-any? #(str/includes? % "spawn-session") @calls))))))

(deftest reaper-stops-only-owned-jvms-with-deleted-directories
  (let [live-dir (temp-dir)
        deleted-dir (io/file live-dir "deleted")
        _ (.mkdirs deleted-dir)
        missing (str deleted-dir)
        _ (.delete deleted-dir)
        sessions [{:name "orphan" :owned? true :repl-type "clj" :project-dir missing}
                  {:name "live" :owned? true :repl-type "clj" :project-dir live-dir}
                  {:name "external" :owned? false :repl-type "clj" :project-dir missing}
                  {:name "native" :owned? true :repl-type "cljw" :project-dir missing}
                  {:name "no-dir" :owned? true :repl-type "clj"}]
        stopped (atom [])
        shouts (atom [])
        result (admission/reap!
                {:sessions-fn (constantly sessions)
                 :stop-fn (fn [session] (swap! stopped conj (:name session)) true)
                 :shout-fn #(swap! shouts conj %)})]
    (is (= ["orphan"] @stopped))
    (is (= ["orphan"] (mapv :name (:stopped result))))
    (is (= 1 (count @shouts)))
    (is (str/includes? (first @shouts) missing))))

(deftest failed-stop-is-not-claimed-as-reaped
  (let [missing (str (temp-dir) "/gone")
        shouts (atom [])
        result (admission/reap!
                {:sessions-fn (constantly
                               [{:name "busy" :owned? true :repl-type "clj" :project-dir missing}])
                 :stop-fn (constantly false)
                 :shout-fn #(swap! shouts conj %)})]
    (is (= ["busy"] (:skipped result)))
    (is (empty? @shouts))))

(deftest reaper-verb-uses-the-injected-port
  (let [report {:stopped [{:name "orphan"}] :failed [] :skipped []}
        response (binding [cider/*reap-fn* (fn [] report)]
                   (cider/handle-reap {}))]
    (is (not (:isError response)))
    (is (str/includes? (:text response) "orphan"))
    (is (fn? (get cider/handlers :reap)))))

(deftest shout-goes-through-the-host-tool-port
  (let [calls (atom [])]
    (try
      (ports/configure! {:tools-invoke-fn (fn [tool args] (swap! calls conj [tool args]))})
      (ports/shout! "hive-emacs" "progress" "stopped orphan")
      (is (= [["hivemind" {:command "shout" :agent_id "hive-emacs"
                           :event_type "progress" :message "stopped orphan"}]] @calls))
      (finally (ports/clear!)))))
