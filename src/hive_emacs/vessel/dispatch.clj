(ns hive-emacs.vessel.dispatch
  "The :vessel capability in hive-spi.editor.services: one :dispatch sink
   that runs any hive-vessel op map against this Emacs.

   `(svc/invoke :vessel :dispatch op timeout-ms)` plans OP through the
   standard hive-vessel registry plus hive-emacs's translator sets (one
   sibling namespace per op namespace, e.g. hive-emacs.swarm.translators),
   executes the native elisp on the Emacs vessel target, and answers the
   eval-shaped envelope {:success bool :result <raw string> :error any
   :timed-out bool}."
  (:require [hive-emacs.client :as ec]
            [hive-emacs.swarm.translators :as swarm]
            [hive-emacs.project.translators :as project]
            [hive-emacs.kanban.translators :as kanban]
            [hive-emacs.crystal.translators :as crystal]
            [hive-emacs.vessel :as vessel]
            [hive-spi.editor.services :as svc]
            [hive-vessel.core :as vcore]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def registry-key
  "hive-spi.editor.services key the capability map is published under."
  :vessel)

(def ^:dynamic *eval-fn*
  "Elisp evaluation boundary: (f code timeout-ms) ->
   {:success bool :result any :error any :timed-out bool}. Defaults to the
   bridge-aware evaluator, resolved per call; tests bind a recording stub."
  (fn [code timeout-ms]
    (vessel/bridge-evaluator code timeout-ms)))

(def translators
  "Every op translator set hive-emacs contributes, in registration order:
   the :swarm/* / :cider/* ops from hive-emacs.swarm.translators, :project/*
   from hive-emacs.project.translators, :kanban/* from
   hive-emacs.kanban.translators, and :crystal/* from
   hive-emacs.crystal.translators."
  (vec (concat swarm/translators project/translators kanban/translators crystal/translators)))

(defn registry
  "The hive-vessel registry :dispatch plans against."
  []
  (vcore/standard-registry translators))

(defn- envelope
  "Eval-shaped envelope from dispatch! OUTCOME and the LAST raw eval
   response (nil when nothing was evaluated)."
  [outcome last-resp]
  (if-let [p (:ok outcome)]
    {:success true
     :result (last (:plan/results p))
     :error nil
     :timed-out (boolean (:timed-out last-resp))}
    (if (and last-resp (not (:success last-resp)))
      {:success false
       :result (:result last-resp)
       :error (:error last-resp)
       :timed-out (boolean (:timed-out last-resp))}
      {:success false
       :result nil
       :error (:error outcome)
       :timed-out false})))

(defn dispatch
  "Run OP (a hive-vessel op map, or a sequence of them) on this Emacs with
   TIMEOUT-MS per native eval (nil = the client's default timeout).
   Returns {:success :result :error :timed-out}; on a planning failure
   :error is the hive-vessel failure map."
  [op timeout-ms]
  (let [last-resp (volatile! nil)
        eval-fn (fn [code t]
                  (let [r (*eval-fn* code t)]
                    (vreset! last-resp r)
                    r))
        target (vessel/target {:eval-fn eval-fn
                               :timeout-ms (or timeout-ms ec/*default-timeout-ms*)})]
    (envelope (vcore/dispatch! (registry) target op) @last-resp)))

(def capabilities
  "Capability map published under `registry-key`."
  {:dispatch dispatch})

(defn register!
  "Publish the capability map under :vessel. Idempotent."
  []
  (svc/register-services! registry-key capabilities))

(defn unregister!
  "Drop the :vessel capabilities. No-op when absent."
  []
  (svc/unregister-services! registry-key))
