(ns hive-emacs.editor.port-test
  "The EmacsEditorPort must satisfy the hive-spi editor contract, delegate to
   the verb handlers, and report EXACTLY the optional surfaces it implements —
   claiming :daemon it cannot serve would make a host dispatch into an
   AbstractMethodError."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is use-fixtures]]
            [hive-emacs.editor.port :as port]
            [hive-emacs.tools.emacs :as emacs]
            [hive-spi.editor.ports :as ports]
            [hive-spi.editor.registry :as registry]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(use-fixtures :each (fn [f]
                      (registry/reset-registry!)
                      (f)
                      (registry/reset-registry!)))

(defn- recording-eval-fn
  [calls]
  (fn
    ([code] (swap! calls conj code) {:success true :result "\"{}\""})
    ([code _timeout] (swap! calls conj code) {:success true :result "\"{}\""})))

(deftest port-satisfies-substrate-contract
  (is (satisfies? ports/IEditorPort (port/->port))))

(deftest port-reports-only-implemented-surfaces
  (let [p (port/->port)]
    (is (= #{:buffer :docs} (registry/surfaces p))
        "daemon verbs have no handlers yet — the surface must not be claimed")
    (is (registry/supports? p :buffer))
    (is (registry/supports? p :docs))
    (is (not (registry/supports? p :daemon)))))

(deftest registry-roundtrip
  (is (not (registry/port-set?)))
  (port/register!)
  (is (registry/port-set?))
  (is (satisfies? ports/IEditorPort (registry/get-port)))
  (port/unregister!)
  (is (not (registry/port-set?))))

(deftest substrate-methods-delegate-to-verb-handlers
  (let [calls (atom [])]
    (binding [emacs/*eval-fn* (recording-eval-fn calls)]
      (let [p (port/->port)]
        (ports/editor-eval p {:code "(+ 1 2)"})
        (ports/editor-notify p {:message "hi" :level "info"})
        (is (= 2 (count @calls)))
        (is (str/includes? (nth @calls 0) "(+ 1 2)"))
        (is (str/includes? (nth @calls 1) "hi"))))))

(deftest buffer-methods-delegate-to-verb-handlers
  (let [calls (atom [])]
    (binding [emacs/*eval-fn* (recording-eval-fn calls)]
      (let [p (port/->port)]
        (ports/list-buffers p {})
        (ports/switch-buffer p {:buffer "*scratch*"})
        (ports/find-file p {:file "/tmp/x.clj"})
        (is (str/includes? (nth @calls 0) "buffer-list"))
        (is (str/includes? (nth @calls 1) "switch-to-buffer"))
        (is (str/includes? (nth @calls 2) "find-file"))))))

(deftest dispatch-through-registry
  (let [calls (atom [])]
    (binding [emacs/*eval-fn* (recording-eval-fn calls)]
      (port/register!)
      (ports/editor-eval (registry/get-port) {:code "(emacs-version)"})
      (is (some #(str/includes? % "(emacs-version)") @calls)))))
