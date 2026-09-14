(ns hive-emacs.vessel
  "Host-neutral Emacs vessel capability.

   A host adapter may translate this descriptor into its own vessel protocol.
   Context and capability lookups are injected runtime ports."
  (:require [hive-emacs.runtime-ports :as ports]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defn create-emacs-vessel
  "Return a neutral vessel descriptor with no host implementation dependency."
  []
  {:vessel/id :emacs
   :vessel/capabilities #{:terminal :editor :delivery :repl}
   :vessel/resolve-context
   (fn [agent-id]
     (when agent-id
       (try
         (ports/resolve-agent-context agent-id)
         (catch Exception e
           (log/debug "Emacs vessel context resolution failed"
                      {:agent-id agent-id :error (ex-message e)})
           nil))))
   :vessel/addon
   (fn [capability]
     (when (contains? #{:terminal :editor :delivery :repl} capability)
       (ports/capability capability)))
   :vessel/initialize!
   (fn [config]
     (log/info "Emacs vessel initialized"
               (when config {:config-keys (keys config)}))
     nil)
   :vessel/shutdown!
   (fn []
     (log/info "Emacs vessel shut down")
     nil)})

(def dialect
  "The hive-vessel dialect this vessel speaks."
  :elisp)

(def target-hook-key
  "IAddon hook key: a zero-arg fn returning the hive-vessel target for this
   Emacs (the same key hive-vscode and hive-vim use)."
  :vessel/target)

(defn execute!
  "Evaluate one :elisp native op through EVAL-FN, `(fn [code timeout-ms])`
   answering {:success :result :error} as the bridge-aware evaluator does.
   Returns the result; throws on failure so hive-vessel's dispatch! reports
   :execute-threw with how many ops of the batch already ran."
  [eval-fn timeout-ms {:native/keys [dialect payload]}]
  (when-not (= :elisp dialect)
    (throw (ex-info "the Emacs vessel executes :elisp only" {:dialect dialect})))
  (let [{:keys [success result] :as r} (eval-fn payload timeout-ms)]
    (if success
      result
      (throw (ex-info (str "emacs vessel: " (:error r))
                      (assoc (select-keys r [:error :timed-out :circuit-open])
                             :payload payload))))))

(defn bridge-evaluator
  "The bridge-aware evaluator, hive-emacs.dsl.ext-hooks/eval-elisp-with-timeout,
   resolved at call time: that namespace requires this one, so a load-time
   require here would be a cycle, and a value captured at target build time
   would miss a reload."
  [code timeout-ms]
  ((requiring-resolve 'hive-emacs.dsl.ext-hooks/eval-elisp-with-timeout) code timeout-ms))

(defn target
  "A hive-vessel target for this Emacs: {:vessel/id :emacs :vessel/dialect
   :elisp :vessel/execute! f}. opts: :eval-fn (default `bridge-evaluator`),
   :timeout-ms (default 10000), :features advertised to translator guards."
  ([] (target {}))
  ([{:keys [eval-fn timeout-ms features]}]
   (let [eval-fn (or eval-fn bridge-evaluator)
         timeout-ms (or timeout-ms 10000)]
     (cond-> {:vessel/id :emacs
              :vessel/dialect dialect
              :vessel/execute! (fn [op] (execute! eval-fn timeout-ms op))}
       features (assoc :vessel/features features)))))
