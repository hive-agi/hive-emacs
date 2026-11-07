(ns hive-emacs.claude-terminal-test
  "The :claude terminal over a stub evaluator: no Emacs, no host."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-addon.terminal :as term]
            [hive-emacs.bridge-loader :as loader]
            [hive-emacs.claude-terminal :as ct]))

;; SPDX-License-Identifier: MIT

(use-fixtures :each
  (fn [t] (loader/invalidate-ready!) (ct/reset-load-state!) (t)))

(defn- emacs
  "Evaluator double. `answers` maps a substring of the code to the result
   map; anything else succeeds with \"t\". Every code string lands in `seen`."
  [seen answers]
  (fn [code _timeout]
    (swap! seen conj code)
    (or (some (fn [[k v]] (when (str/includes? code k) v)) answers)
        {:success true :result "t"})))

(def ^:private live {"emacs-pid" {:success true :result "\"42|1.0\""}})

(deftest the-id-is-unchanged-so-live-lings-keep-resolving
  (is (= :claude (term/terminal-id (ct/make-claude-terminal (emacs (atom []) live))))))

(deftest spawn-elisp-quotes-everything-the-agent-supplies
  (let [code (ct/spawn-elisp "ling-1" "x\" (shell-command \"rm\")" ["a" "b"] "/w d" nil)]
    (is (str/starts-with? code "(json-encode (hive-claude-bridge-api-spawn \"ling-1\" "))
    (is (str/includes? code "\"x\\\" (shell-command \\\"rm\\\")\"") "the name stays one string literal")
    (is (str/includes? code "'(\"a\" \"b\")"))
    (is (str/includes? code "\"/w d\" 1 nil nil))"))))

(deftest status-merges-the-bridge-answer
  (let [t (ct/make-claude-terminal
           (emacs (atom []) (assoc live "bridge-api-status"
                                   {:success true :result "{\"buffer-alive\":true,\"status\":\"idle\"}"})))]
    (is (= {:x 1 :slave/id "l" :elisp-alive? true :terminal-status "idle"}
           (term/terminal-status t {:id "l"} {:x 1})))))

(deftest kill-reports-each-outcome
  (doseq [[why answer expected] [["killed"  {:success true :result "{\"killed\":true}"}  {:killed? true :id "l"}]
                                 ["blocked" {:success true :result "{\"killed\":false}"} {:killed? false :id "l" :reason :kill-blocked}]
                                 ["error"   {:success false :error "down"}              {:killed? false :id "l" :reason :elisp-error :error "down"}]]]
    (testing why
      (is (= expected (term/terminal-kill! (ct/make-claude-terminal (emacs (atom []) (assoc live "bridge-api-kill" answer)))
                                           {:id "l"}))))))

(deftest a-failed-spawn-throws
  (let [t (ct/make-claude-terminal (emacs (atom []) (assoc live "bridge-api-spawn" {:success false :error "no"})))]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"spawn failed" (term/terminal-spawn! t {:id "l"} {})))))

(defn- scripted
  "Evaluator double whose answer to code containing a key of `scripts` is
   the next entry of that key's vector (the last one repeats); otherwise
   as `emacs`."
  [seen scripts answers]
  (let [left (atom scripts)]
    (fn [code t]
      (swap! seen conj code)
      (if-let [k (some #(when (str/includes? code %) %) (keys @left))]
        (let [[a & more] (get @left k)]
          (when (seq more) (swap! left assoc k (vec more)))
          a)
        ((emacs (atom []) answers) code t)))))

(defn- alive [buffer? process?]
  {:success true :result (format "{\"buffer-alive\":%s,\"process-alive\":%s}" buffer? process?)})

(def ^:private fast {:sleep-fn (fn [_]) :spawn-polls 3 :dispatch-polls 4})

(defn- spawned [scripts]
  (let [seen (atom [])
        t    (ct/make-claude-terminal (scripted seen scripts live) nil fast)]
    {:seen seen
     :result (try (term/terminal-spawn! t {:id "l"} {})
                  (catch clojure.lang.ExceptionInfo e e))}))

(deftest spawn-succeeds-only-once-the-terminal-process-is-alive
  (is (= "l" (:result (spawned {"bridge-api-alive" [(alive true false) (alive true true)]})))))

(deftest spawn-throws-when-the-terminal-process-never-starts
  (let [{:keys [result seen]} (spawned {"bridge-api-alive" [(alive true false)]})]
    (is (instance? clojure.lang.ExceptionInfo result))
    (is (re-find #"process" (str (ex-message result))))
    (is (= :terminal-not-alive (:reason (ex-data result))))
    (is (some #(str/includes? % "bridge-api-kill") @seen) "the dead session is cleaned up")))

(deftest spawn-throws-when-the-buffer-is-gone
  (is (= :terminal-not-alive (:reason (ex-data (:result (spawned {"bridge-api-alive" [(alive false false)]})))))))

(deftest spawn-throws-when-the-bridge-cannot-load
  (let [seen (atom [])
        t    (ct/make-claude-terminal
              (emacs seen (assoc live "(require 'hive-claude-config)" {:success false :error "no file"}))
              nil fast)]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"did not load" (term/terminal-spawn! t {:id "l"} {})))
    (is (not-any? #(str/includes? % "bridge-api-spawn") @seen) "nothing is spawned on an unloaded bridge")))

(defn- paste [state] {:success true :result (format "{\"buffer-alive\":true,\"paste\":\"%s\"}" state)})

(def ^:private sent {:success true :result "{\"sent\":true}"})

(defn- dispatched [scripts task & [opts]]
  (let [seen (atom [])
        t    (ct/make-claude-terminal (scripted seen scripts live) nil (merge fast opts))]
    {:seen seen :result (term/terminal-dispatch! t {:id "l"} {:task task})}))

(deftest dispatch-returns-true-once-the-paste-is-delivered
  (is (true? (:result (dispatched {"bridge-api-dispatch" [sent]
                                   "bridge-api-status"   [(paste "pasting") (paste "delivered")]}
                                  "do it")))))

(deftest dispatch-refuses-what-it-could-not-deliver
  (doseq [[why scripts reason] [["bridge says failed"  {"bridge-api-dispatch" [sent] "bridge-api-status" [(paste "failed")]} :paste-failed]
                                ["paste never settles" {"bridge-api-dispatch" [sent] "bridge-api-status" [(paste "pasting")]} :paste-timeout]
                                ["no live buffer"      {"bridge-api-dispatch" [{:success true :result "null"}]} :no-live-session]]]
    (testing why
      (let [{:keys [result]} (dispatched scripts "do it")]
        (is (false? (:dispatched result)))
        (is (= reason (:reason result)))))))

(deftest dispatch-refuses-a-task-above-the-limit-without-pasting-any-of-it
  (let [{:keys [result seen]} (dispatched {} (apply str (repeat 101 "x")) {:max-task-chars 100})]
    (is (false? (:dispatched result)))
    (is (= :task-too-large (:reason result)))
    (is (= {:chars 101 :max-chars 100} (select-keys result [:chars :max-chars])))
    (is (not-any? #(str/includes? % "bridge-api-dispatch") @seen))))

(deftest features-load-once-per-emacs-session
  (let [seen (atom [])
        fp   (atom "\"42|1.0\"")
        f    (emacs seen {"emacs-pid"   {:success true :result "\"42|1.0\""}
                          "(featurep 'hive-claude-bridge)" {:success true :result "\"t\""}})
        eval (fn [code t] (if (str/includes? code "emacs-pid") {:success true :result @fp} (f code t)))
        loads #(count (filter (fn [c] (str/includes? c "(require 'hive-claude-config)")) @seen))]
    (is (true? (ct/ensure-loaded! eval)))
    (is (true? (ct/ensure-loaded! eval)))
    (is (= 1 (loads)) "same session: loaded once")
    (reset! fp "\"43|2.0\"")
    (is (true? (ct/ensure-loaded! eval)))
    (is (= 2 (loads)) "a restarted Emacs gets the features again")))

(deftest registration-goes-through-the-host-terminal-ports
  (testing "no :terminal/register! port: a quiet no"
    (is (false? (ct/register! {} (emacs (atom []) live))))
    (is (nil? (ct/unregister! {} (emacs (atom []) live)))))
  (testing "the ports receive the :claude terminal and its removal"
    (let [calls (atom [])
          ports {:terminal/register!    (fn [id t] (swap! calls conj [:register id (term/terminal-id t)]) {:registered? true})
                 :terminal/deregister!  (fn [id] (swap! calls conj [:deregister id]))
                 :catchup/spawn-context (constantly "ctx")}]
      (is (true? (ct/register! ports (emacs (atom []) live))))
      (ct/unregister! ports (emacs (atom []) live))
      (is (= [[:register :claude :claude] [:deregister :claude]] @calls)))))
