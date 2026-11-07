(ns hive-emacs.cider.eval-direct-route-test
  "Card 20260727234852-219ab332: `code cider eval` failed with
   \"Emacsclient call timed out after 5000ms\" while the nREPL on the named
   port answered at once. An eval that names a port goes straight to that
   nREPL and makes no emacsclient call; only an unreachable port falls back
   to the Emacs session route."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-emacs.cider.eval-port :as eval-port]
            [hive-emacs.tools.cider :as cider]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defn- timing-out-emacs [calls]
  (fn [& _]
    (swap! calls inc)
    {:success false :timed-out true
     :error "Emacsclient call timed out after 5000ms"}))

(defn- run [emacs direct params]
  (binding [cider/*eval-fn*          emacs
            cider/*attention-fn*     (constantly nil)
            cider/*direct-eval-fn*   direct
            eval-port/*port-file-fn* {"/wt" "7946\n"}]
    (let [resp (cider/handle-eval params)]
      {:error? (boolean (:isError resp))
       :text   (str (:text resp) (get-in resp [:content 0 :text]))})))

(deftest a-named-port-evaluates-directly-even-when-emacs-times-out
  (let [emacs-calls  (atom 0)
        direct-calls (atom [])
        out (run (timing-out-emacs emacs-calls)
                 (fn [port code opts]
                   (swap! direct-calls conj [port code opts])
                   {:ok "3"})
                 {:code "(+ 1 2)" :directory "/wt" :timeout 7})]
    (is (not (:error? out)) (:text out))
    (is (str/includes? (:text out) "3"))
    (is (= [[7946 "(+ 1 2)" {:timeout-ms 7000}]] @direct-calls))
    (is (zero? @emacs-calls) "no emacsclient call on the direct route")))

(deftest the-port-param-wins-and-defaults-to-a-60s-deadline
  (let [direct-calls (atom [])
        out (run (timing-out-emacs (atom 0))
                 (fn [port code opts] (swap! direct-calls conj [port opts]) {:ok "nil"})
                 {:code "nil" :directory "/wt" :port 7000})]
    (is (not (:error? out)) (:text out))
    (is (= [[7000 {:timeout-ms 60000}]] @direct-calls))))

(deftest an-eval-error-from-the-nrepl-is-reported-not-retried-through-emacs
  (let [emacs-calls (atom 0)
        out (run (timing-out-emacs emacs-calls)
                 (fn [_ _ _] {:error :cider/eval-error :message "boom"})
                 {:code "(throw (ex-info \"boom\" {}))" :directory "/wt"})]
    (is (:error? out))
    (is (str/includes? (:text out) "boom"))
    (is (zero? @emacs-calls))))

(deftest an-unreachable-port-falls-back-to-the-emacs-route
  (let [emacs-calls (atom 0)
        out (run (timing-out-emacs emacs-calls)
                 (fn [port _ _] {:error :cider/port-unreachable
                                 :message (str "nothing on " port)})
                 {:code "1" :directory "/wt"})]
    (is (:error? out))
    (is (pos? @emacs-calls) "the session route was tried")))
