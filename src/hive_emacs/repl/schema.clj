;; PROPRIETARY - Copyright 2026 BuddhiLW. All Rights Reserved.
;; This file is part of hive-emacs and may not be distributed
;; without explicit written permission.

(ns hive-emacs.repl.schema
  "Malli value objects for the REPL-backend translation layer.

   One vocabulary spans both transports: a caller names a VERB and a BACKEND,
   and a backend PROFILE says which RPC that verb becomes. Everything here is
   data — no protocol, no function, appears in a schema."
  (:require [malli.core :as m]
            [hive-schemas.schema :as schemas]))

;;; =============================================================================
;;; Identifiers
;;; =============================================================================

(def BackendId
  "A transport identifier. :cider drives an nREPL connection through CIDER,
   :slynk drives a Common Lisp image through SLY.

   Deliberately any keyword rather than an enum of the two: a backend becomes
   real by being registered, and closing this would make the registry's swap
   point unreachable from outside this namespace."
  :keyword)

(def Verb
  "The verb vocabulary, shared by every backend. A backend that cannot serve a
   verb omits it from its ops map rather than defining a failing handler."
  [:enum :eval :doc :info :complete :apropos :status :load-file
   :inspect :restart])

(def Lang
  "Source dialect of an :eval request. :cl is a Common Lisp form evaluated as
   is; :clojure is Clojure source needing a reader/compiler wrapper before the
   host can evaluate it."
  [:enum :cl :clojure])

(def ParamKey
  "A key an op may draw its RPC arguments from."
  [:enum :code :symbol :prefix :package :pattern :filename :form :level :n
   :external-only :case-sensitive])

;;; =============================================================================
;;; Profile — the measured behaviour of one backend, as data
;;; =============================================================================

(def RpcName
  "A package-qualified callee. For :slynk the package qualification is load
   bearing and must be the one verified in the running image, not the one the
   source tree suggests.

   Carries its own generator: the qualification is too sparse in random strings
   for such-that to find it."
  [:and
   {:gen/schema [:tuple
                 [:enum "slynk" "slynk-completion" "slynk-apropos" "nrepl"]
                 [:enum "eval" "info" "describe-symbol" "documentation-symbol"
                  "simple-completions" "apropos-list-for-emacs" "connection-info"]]
    :gen/fmap (fn [[pkg op]] (str pkg ":" op))}
   :string
   [:fn {:error/message "must be package-qualified"}
    (fn [s] (boolean (re-find #"[:/]" s)))]])

(def Op
  "How one verb becomes one call on one backend.

   :op/args names the request keys to pass, positionally. :op/defaults supplies
   values for keys the caller omitted. :op/shape labels the delivered result so
   a decoder can be chosen without re-inspecting the value."
  [:map {:closed true}
   [:op/rpc RpcName]
   [:op/args [:vector ParamKey]]
   [:op/defaults {:optional true} [:map-of ParamKey :any]]
   [:op/shape [:enum :string :string-pair :plist :plist-list :completion-list
               :flex-list :any]]
   [:op/timeout-ms {:optional true} pos-int?]])

(def LangWrapper
  "A format template turning source of one Lang into a form the backend can
   evaluate. `%s` is the source. Absent means the source needs no wrapping."
  [:map {:closed true}
   [:lang/template :string]])

(def Profile
  "The measured behaviour of one backend. Swapping a backend is registering one
   of these — no code above the registry changes (DIP/OCP).

   :profile/prelude are modules the transport must load before its ops resolve;
   on Slynk an op whose package is not yet loaded does not error, it hangs."
  [:map {:closed true}
   [:profile/id BackendId]
   [:profile/label :string]
   [:profile/ops [:map-of Verb Op]]
   [:profile/langs {:optional true} [:map-of Lang LangWrapper]]
   [:profile/prelude {:optional true} [:vector :string]]
   [:profile/default-timeout-ms pos-int?]])

;;; =============================================================================
;;; Request / plan / result
;;; =============================================================================

(def Request
  "A caller's intent, before any backend is consulted. Modelled permissively:
   MCP hands every param through as a string."
  [:map
   [:req/verb Verb]
   [:req/backend BackendId]
   [:req/params {:optional true} [:map-of ParamKey :any]]
   [:req/lang {:optional true} Lang]
   [:req/timeout-ms {:optional true} pos-int?]])

(def Call
  "One resolved RPC: callee plus positional arguments, ready for the boundary.
   Closed — a plan is internal, so an unexpected key is a defect."
  [:map {:closed true}
   [:call/rpc RpcName]
   [:call/args [:vector :any]]
   [:call/shape [:enum :string :string-pair :plist :plist-list :completion-list
                 :flex-list :any]]
   [:call/timeout-ms pos-int?]])

(def Plan
  "The full effect a request translates to: modules to ensure, then the call.
   The boundary executes it; nothing here performs an effect."
  [:map {:closed true}
   [:plan/prelude [:vector :string]]
   [:plan/call Call]])

(def Failure
  "Why a request could not be planned or executed. :unsupported-verb means the
   backend's profile has no op for it; :timeout means the transport gave up,
   which on Slynk is the expected shape of an unresolvable callee."
  [:map {:closed true}
   [:fail/kind [:enum :unknown-backend :unsupported-verb :missing-param
                :timeout :transport]]
   [:fail/message :string]
   [:fail/detail {:optional true} :any]])

(m/=> valid-profile? [:=> [:cat :any] :boolean])

(defn valid-profile?
  "Does X conform to Profile?"
  [x]
  (m/validate Profile x))

(m/=> valid-plan? [:=> [:cat :any] :boolean])

(defn valid-plan?
  "Does X conform to Plan?"
  [x]
  (m/validate Plan x))

(def schema-bundle
  "Registry entries for the REPL translation vocabulary."
  {:hive-emacs.repl/backend-id BackendId
   :hive-emacs.repl/verb Verb
   :hive-emacs.repl/lang Lang
   :hive-emacs.repl/param-key ParamKey
   :hive-emacs.repl/rpc-name RpcName
   :hive-emacs.repl/op Op
   :hive-emacs.repl/lang-wrapper LangWrapper
   :hive-emacs.repl/profile Profile
   :hive-emacs.repl/request Request
   :hive-emacs.repl/call Call
   :hive-emacs.repl/plan Plan
   :hive-emacs.repl/failure Failure})

(schemas/register-all! schema-bundle)
