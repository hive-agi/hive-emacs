(ns hive-emacs.editor.services-test
  "The published capability map is the host's ONLY route to this addon's
   function-level surface. Two things must hold: every capability the host
   used to call directly is present, and every value is actually callable —
   a typo in this map is a NullPointerException in the host at runtime."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [hive-emacs.editor.services :as services]
            [hive-spi.editor.services :as svc]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(use-fixtures :each (fn [f] (svc/reset-services!) (f) (svc/reset-services!)))

(def ^:private host-facing-capabilities
  "The surface hive-mcp reached for through its emacs.client / emacs.elisp
   shims. Dropping one of these silently breaks a host call site."
  #{:eval-elisp :eval-elisp! :eval-elisp-with-timeout :default-timeout-ms
    :emacs-running? :buffer-list :current-buffer :current-file
    :project-root :recent-files
    :switch-to-buffer :find-file :save-buffer :goto-line :insert-text
    :emit :emit-forms :wrap-progn :format-elisp
    :require-and-call :require-and-call-json :require-and-call-plist-json
    :require-and-call-text :fboundp-call-json})

(deftest every-host-facing-capability-is-published
  (is (empty? (remove (set (keys services/capabilities))
                      host-facing-capabilities))))

(deftest every-capability-is-callable
  (doseq [[k f] services/capabilities]
    (is (ifn? f) (str k " is not callable"))))

(deftest register-publishes-into-the-spi-registry
  (is (empty? (svc/capabilities)))
  (services/register!)
  (is (= (set (keys services/capabilities)) (svc/capabilities)))
  (is (svc/available? :eval-elisp))
  (services/unregister!)
  (is (empty? (svc/capabilities))))

(deftest elisp-construction-is-reachable-through-the-registry
  (services/register!)
  (is (= "(progn\n  (a)\n  (b))" (svc/invoke-default :wrap-progn "(a)" "(b)"))
      "wrap-progn is variadic — invoke must pass args through, not as a seq"))

(deftest default-timeout-is-a-thunk-not-a-captured-value
  (services/register!)
  (is (pos? (svc/invoke-default :default-timeout-ms))))
