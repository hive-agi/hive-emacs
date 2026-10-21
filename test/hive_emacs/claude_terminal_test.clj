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

(deftest registering-without-a-host-is-a-quiet-no
  (if (try (requiring-resolve 'hive-mcp.agent.ling.terminal-registry/register-terminal!) (catch Exception _ nil))
    (is true "a host is on this classpath; the absent case cannot be observed here")
    (do (is (false? (ct/register! (emacs (atom []) live))))
        (is (nil? (ct/unregister! (emacs (atom []) live)))))))
