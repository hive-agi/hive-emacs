(ns hive-emacs.cider.port-test
  "The EmacsCiderPort must satisfy the hive-spi ICiderPort contract and stay
   a pure delegation to the verb handlers — the registry is the only state."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is use-fixtures]]
            [hive-emacs.cider.port :as port]
            [hive-emacs.tools.cider :as cider]
            [hive-spi.cider.ports :as ports]
            [hive-spi.cider.registry :as registry]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(use-fixtures :each (fn [f]
                      (registry/reset-registry!)
                      (f)
                      (registry/reset-registry!)))

(deftest port-satisfies-contract
  (is (satisfies? ports/ICiderPort (port/->port))))

(deftest registry-roundtrip
  (is (not (registry/port-set?)))
  (port/register!)
  (is (registry/port-set?))
  (is (satisfies? ports/ICiderPort (registry/get-port)))
  (port/unregister!)
  (is (not (registry/port-set?))))

(deftest methods-delegate-to-verb-handlers
  (let [calls (atom [])]
    (binding [cider/*eval-fn* (fn
                                ([code]
                                 (swap! calls conj code)
                                 {:success true :result "\"{}\""})
                                ([code _]
                                 (swap! calls conj code)
                                 {:success true :result "\"{}\""}))]
      (let [p (port/->port)]
        (ports/cider-status p {})
        (ports/cider-doc p {:symbol "map"})
        (ports/kill-session p {:session_name "s1"})
        (is (= 3 (count @calls)))
        (is (str/includes? (nth @calls 0) "hive-mcp-cider-status"))
        (is (str/includes? (nth @calls 1) "hive-mcp-cider-doc"))
        (is (str/includes? (nth @calls 2) "hive-mcp-cider-kill-session"))))))

(deftest dispatch-through-registry
  (let [calls (atom [])]
    (binding [cider/*eval-fn* (fn
                                ([code]
                                 (swap! calls conj code)
                                 {:success true :result "\"[]\""})
                                ([code _]
                                 (swap! calls conj code)
                                 {:success true :result "\"[]\""}))]
      (port/register!)
      (ports/list-sessions (registry/get-port) {})
      (is (some #(str/includes? % "hive-mcp-cider-list-sessions") @calls)))))
