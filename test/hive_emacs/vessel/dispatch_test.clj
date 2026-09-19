(ns hive-emacs.vessel.dispatch-test
  "Swarm ops lower to exact :elisp natives through the hive-vessel registry,
   and the :vessel :dispatch capability turns a vessel dispatch into the
   eval-shaped envelope, keeping :timed-out."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.string :as str]
            [hive-emacs.swarm.translators :as swarm]
            [hive-emacs.vessel.dispatch :as vd]
            [hive-spi.editor.services :as svc]
            [hive-vessel.core :as vcore]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defn- restore-vessel-key
  "Snapshot the :vessel registration, register ours, run F, then put the
   snapshot back exactly. Never touches other registry keys."
  [f]
  (let [before (get (svc/registered) vd/registry-key)]
    (try
      (vd/register!)
      (f)
      (finally
        (svc/unregister-services! vd/registry-key)
        (when before
          (svc/register-services! vd/registry-key before))))))

(use-fixtures :each restore-vessel-key)

(def ^:private emacs (:emacs vcore/reference-targets))

(defn- payloads
  "The native :elisp payloads OP plans to for the Emacs target."
  [op]
  (let [{:keys [ok error]} (vcore/plan (vd/registry) emacs op)]
    (when error (throw (ex-info "plan failed" error)))
    (mapv (fn [{:native/keys [dialect payload]}]
            (is (= :elisp dialect))
            payload)
          (:plan/ops ok))))

(def ^:private op->payload
  [[{:op :swarm/available?}
    "(featurep 'hive-mcp-swarm)"]
   [{:op :swarm/list-lings}
    "(json-encode (hive-mcp-swarm-list-lings))"]
   [{:op :swarm/status :slave-id "ling-1"}
    "(json-encode (hive-mcp-swarm-status \"ling-1\"))"]
   [{:op :swarm/status :slave-id nil}
    "(json-encode (hive-mcp-swarm-api-status))"]
   [{:op :swarm/status}
    "(json-encode (hive-mcp-swarm-api-status))"]
   [{:op :swarm/broadcast :prompt "hello all"}
    "(json-encode (hive-mcp-swarm-broadcast \"hello all\"))"]
   [{:op :swarm/list-presets}
    "(json-encode (hive-mcp-swarm-api-list-presets))"]
   [{:op :swarm/pending-prompts}
    "(json-encode (hive-mcp-swarm-api-pending-prompts))"]
   [{:op :swarm/respond-prompt :slave-id "ling-2" :response "yes"}
    "(json-encode (hive-mcp-swarm-api-respond-prompt \"ling-2\" \"yes\"))"]
   [{:op :swarm/kill :slave-id "ling-3"}
    "(when (fboundp 'hive-mcp-swarm-slaves-kill) (hive-mcp-swarm-slaves-kill \"ling-3\"))"]
   [{:op :swarm/send-prompt :slave-id "ling-4" :prompt "do it"}
    "(hive-mcp-swarm-send-to-terminal \"ling-4\" \"do it\")"]
   [{:op :swarm/slave-ready? :slave-id "ling-5"}
    "(if (hive-mcp-swarm-tasks--slave-ready-p \"ling-5\") \"t\" \"nil\")"]
   [{:op :swarm/collect :task-id "task-9" :timeout-ms 30000}
    "(json-encode (hive-mcp-swarm-api-collect \"task-9\" 30000))"]
   [{:op :swarm/collect :task-id "task-9" :timeout-ms nil}
    "(json-encode (hive-mcp-swarm-api-collect \"task-9\" nil))"]
   [{:op :cider/spawn-session :name "s1"}
    "(json-encode (hive-mcp-cider-spawn-session \"s1\" nil nil))"]
   [{:op :cider/kill-session :name "s1"}
    "(hive-mcp-cider-kill-session \"s1\")"]])

(deftest every-op-lowers-to-the-exact-native-payload
  (doseq [[op expected] op->payload]
    (testing (pr-str op)
      (is (= [expected] (payloads op))))))

(deftest every-translator-is-covered
  (is (= (set (map :translator/op swarm/translators))
         (set (map (comp :op first) op->payload))))
  (is (every? (set vd/translators) swarm/translators)))

(deftest interpolations-are-escaped-by-the-dialect
  (let [nasty "a\"b\\c"
        lit "\"a\\\"b\\\\c\""]
    (is (= [(str "(json-encode (hive-mcp-swarm-broadcast " lit "))")]
           (payloads {:op :swarm/broadcast :prompt nasty})))
    (is (= [(str "(json-encode (hive-mcp-swarm-api-respond-prompt " lit " " lit "))")]
           (payloads {:op :swarm/respond-prompt :slave-id nasty :response nasty})))
    (is (= [(str "(hive-mcp-swarm-send-to-terminal " lit " " lit ")")]
           (payloads {:op :swarm/send-prompt :slave-id nasty :prompt nasty})))
    (is (= [(str "(hive-mcp-cider-kill-session " lit ")")]
           (payloads {:op :cider/kill-session :name nasty})))))

(deftest malformed-ops-do-not-plan
  (doseq [op [{:op :swarm/broadcast}
              {:op :swarm/kill :slave-id 7}
              {:op :swarm/collect :task-id "t" :timeout-ms "soon"}
              {:op :swarm/slave-ready?}
              {:op :swarm/slave-ready? :slave-id ""}
              {:op :swarm/slave-ready? :slave-id "   "}
              {:op :swarm/slave-ready? :slave-id 5}]]
    (is (contains? (vcore/plan (vd/registry) emacs op) :error) (pr-str op))))

(defn- dispatch-with
  "Invoke :dispatch through the SPI with *eval-fn* bound to a stub answering
   RESPONSE. Returns {:ret envelope :calls [[code timeout-ms] ...]}."
  [response op timeout-ms]
  (let [calls (atom [])]
    (binding [vd/*eval-fn* (fn [code t]
                             (swap! calls conj [code t])
                             response)]
      {:ret (svc/invoke :vessel :dispatch op timeout-ms)
       :calls @calls})))

(deftest dispatch-envelope
  (testing "success: raw result string, evaluated with the op's timeout"
    (let [{:keys [ret calls]} (dispatch-with {:success true :result "[1]"}
                                             {:op :swarm/list-lings} 4321)]
      (is (= [["(json-encode (hive-mcp-swarm-list-lings))" 4321]] calls))
      (is (= {:success true :result "[1]" :error nil :timed-out false} ret))))
  (testing "eval failure keeps the raw error"
    (is (= {:success false :result nil :error "no emacs" :timed-out false}
           (:ret (dispatch-with {:success false :error "no emacs"}
                                {:op :swarm/available?} 2000)))))
  (testing "timeout survives dispatch!"
    (is (= {:success false :result nil :error "timeout" :timed-out true}
           (:ret (dispatch-with {:success false :error "timeout" :timed-out true}
                                {:op :swarm/status :slave-id "a"} 10)))))
  (testing "nil timeout falls back to the client default"
    (is (pos-int? (second (first (:calls (dispatch-with {:success true :result "t"}
                                                        {:op :swarm/available?} nil)))))))
  (testing "an unplannable op never evaluates and fails with the vessel failure"
    (let [{:keys [ret calls]} (dispatch-with {:success true :result "t"}
                                             {:op :swarm/no-such-op} 10)]
      (is (empty? calls))
      (is (false? (:success ret)))
      (is (false? (:timed-out ret)))
      (is (= :unsupported (get-in ret [:error :failure/reason]))))))

(deftest registry-key-is-vessel
  (is (= :vessel vd/registry-key)))

(deftest every-op-namespace-dispatches
  (doseq [[op needle] [[{:op :swarm/slave-ready? :slave-id "s1"} "slave-ready-p"]
                       [{:op :project/info} "projectile-api-project-info"]
                       [{:op :kanban/move-to-done :task-id "t1"} "kanban-move"]
                       [{:op :crystal/available?} "featurep"]
                       [{:op :crystal/git-commits :since "midnight"} "git log"]]]
    (testing (pr-str op)
      (let [{:keys [ret calls]} (dispatch-with {:success true :result "ok"} op 1000)]
        (is (:success ret))
        (is (seq calls))
        (is (some #(str/includes? (first %) needle) calls)
            (pr-str calls))))))

(deftest register-publishes-and-unregister-retracts-the-key
  (is (= #{:dispatch} (svc/capabilities :vessel)))
  (vd/unregister!)
  (is (not (contains? (svc/registered) :vessel)))
  (is (= :editor/capability-unavailable
         (:error (svc/invoke :vessel :dispatch {:op :swarm/list-lings} 1))))
  (vd/register!)
  (is (= #{:dispatch} (svc/capabilities :vessel))))
