;; PROPRIETARY - Copyright 2026 BuddhiLW. All Rights Reserved.
;; This file is part of hive-emacs and may not be distributed
;; without explicit written permission.

(ns hive-emacs.repl.translate
  "Pure translation: a verb-and-backend request becomes an executable plan.

   Nothing here performs an effect or names a transport. Every backend
   difference is read out of the profile, so a new backend needs no change in
   this namespace."
  (:require [hive-dsl.result :as result]
            [hive-emacs.repl.profile :as profile]
            [hive-emacs.repl.schema :as schema]
            [malli.core :as m]))

;;; =============================================================================
;;; Source encoding
;;; =============================================================================

(m/=> cl-string [:=> [:cat :string] :string])

(defn cl-string
  "SRC as a Common Lisp string literal: wrapped in double quotes, with
   backslashes and double quotes escaped."
  [src]
  (str \" (-> src (.replace "\\" "\\\\") (.replace "\"" "\\\"")) \"))

(m/=> wrap-source [:=> [:cat [:maybe schema/LangWrapper] :string] :string])

(defn wrap-source
  "SRC rendered through WRAPPER's template, or SRC unchanged when WRAPPER is
   nil. The template receives SRC as a Common Lisp string literal."
  [wrapper src]
  (if-let [template (:lang/template wrapper)]
    (format template (cl-string src))
    src))

(m/=> lang-wrapper [:=> [:cat schema/Profile [:maybe :keyword] :keyword]
                    [:maybe schema/LangWrapper]])

(defn lang-wrapper
  "The wrapper PROFILE declares for LANG, or nil. Only :eval is wrapped; every
   other verb takes its source uninterpreted."
  [prof lang verb]
  (when (= :eval verb)
    (get-in prof [:profile/langs lang])))

;;; =============================================================================
;;; Argument resolution
;;; =============================================================================

(m/=> resolve-args
      [:=> [:cat schema/Op [:map-of :keyword :any]]
       [:or [:tuple [:= :ok] [:vector :any]] [:tuple [:= :missing] :keyword]]])

(defn resolve-args
  "OP's positional arguments drawn from PARAMS, falling back to :op/defaults.

   Returns [:ok args] or [:missing key]. A default of nil counts as supplied —
   presence is decided by `contains?`, so an op may legitimately default an
   argument to nil."
  [op params]
  (let [defaults (:op/defaults op {})]
    (reduce (fn [_ k]
              (let [[found? v] (cond
                                 (contains? params k) [true (get params k)]
                                 (contains? defaults k) [true (get defaults k)]
                                 :else [false nil])]
                (if found?
                  [:ok (conj (second _) v)]
                  (reduced [:missing k]))))
            [:ok []]
            (:op/args op))))

;;; =============================================================================
;;; Planning
;;; =============================================================================

(m/=> timeout-for [:=> [:cat schema/Profile schema/Op [:maybe pos-int?]] pos-int?])

(defn timeout-for
  "The timeout in force: the request's, else the op's, else the profile's."
  [prof op requested]
  (or requested
      (:op/timeout-ms op)
      (:profile/default-timeout-ms prof)))

(defn- fail
  [kind message detail]
  (result/err kind (cond-> {:fail/kind kind :fail/message message}
                     (some? detail) (assoc :fail/detail detail))))

(m/=> plan [:=> [:cat schema/Request] :any])

(defn plan
  "REQUEST translated against its backend's profile.

   Returns a Result: ok wraps a schema/Plan, err carries a schema/Failure. The
   plan's :plan/prelude lists modules the boundary must ensure are loaded
   before the call is issued."
  [{:req/keys [verb backend params lang timeout-ms] :or {params {}}}]
  (if-let [prof (profile/profile backend)]
    (if-let [op (get-in prof [:profile/ops verb])]
      (let [wrapper (lang-wrapper prof lang verb)
            params' (if (and wrapper (contains? params :code))
                      (update params :code #(wrap-source wrapper %))
                      params)
            [status v] (resolve-args op params')]
        (if (= :ok status)
          (result/ok {:plan/prelude (vec (:profile/prelude prof []))
                      :plan/call {:call/rpc (:op/rpc op)
                                  :call/args v
                                  :call/shape (:op/shape op)
                                  :call/timeout-ms (timeout-for prof op timeout-ms)}})
          (fail :missing-param
                (str "verb " verb " on " backend " requires " v)
                {:verb verb :param v})))
      (fail :unsupported-verb
            (str backend " does not serve " verb)
            {:verb verb :supported (profile/capabilities backend)}))
    (fail :unknown-backend
          (str "no such backend: " backend)
          {:backend backend :known (profile/backends)})))
