;; PROPRIETARY - Copyright 2026 BuddhiLW. All Rights Reserved.
;; This file is part of hive-emacs and may not be distributed
;; without explicit written permission.

(ns hive-emacs.repl.profile
  "Backend profiles and their registry — the DIP swap point.

   A profile is plain data describing what one transport does with each verb.
   Adding a third transport is `register!` of one more profile; nothing above
   this namespace changes.

   The :slynk profile's RPC names and argument lists were measured against a
   running image, and the package qualifications are load bearing."
  (:require [hive-emacs.repl.schema :as schema]
            [malli.core :as m]))

;;; =============================================================================
;;; Profiles
;;; =============================================================================

(def cider-profile
  "nREPL, driven through CIDER. Verbs map to nREPL op names.

   :inspect and :restart are absent: CIDER exposes both, but neither has been
   measured here, and an unmeasured op is not a capability."
  {:profile/id :cider
   :profile/label "CIDER (nREPL)"
   :profile/default-timeout-ms 60000
   :profile/ops
   {:eval      {:op/rpc "nrepl/eval"        :op/args [:code]    :op/shape :string}
    :doc       {:op/rpc "nrepl/info"        :op/args [:symbol]  :op/shape :plist}
    :info      {:op/rpc "nrepl/info"        :op/args [:symbol]  :op/shape :plist}
    :complete  {:op/rpc "nrepl/completions" :op/args [:prefix]  :op/shape :completion-list}
    :apropos   {:op/rpc "nrepl/apropos"     :op/args [:pattern] :op/shape :plist-list}
    :status    {:op/rpc "nrepl/describe"    :op/args []         :op/shape :plist}
    :load-file {:op/rpc "nrepl/load-file"   :op/args [:filename] :op/shape :string}}})

(def slynk-profile
  "SLY/Slynk, driven against a Common Lisp image.

   `slynk-completion:` and `slynk-apropos:` are separate packages from `slynk:`
   and resolve only after :profile/prelude has been required. An op whose
   package has not been loaded does not signal — the request never receives a
   reply — so the prelude is a correctness requirement, not an optimisation.

   :eval uses eval-and-grab-output so a form's stdout reaches the caller
   alongside its value."
  {:profile/id :slynk
   :profile/label "SLY (Slynk)"
   :profile/default-timeout-ms 20000
   :profile/prelude ["slynk/completion" "slynk/apropos"]
   :profile/ops
   {:eval      {:op/rpc "slynk:eval-and-grab-output"
                :op/args [:code] :op/shape :string-pair}
    :doc       {:op/rpc "slynk:documentation-symbol"
                :op/args [:symbol] :op/shape :string}
    :info      {:op/rpc "slynk:describe-symbol"
                :op/args [:symbol] :op/shape :string}
    :complete  {:op/rpc "slynk-completion:simple-completions"
                :op/args [:prefix :package]
                :op/defaults {:package "COMMON-LISP-USER"}
                :op/shape :completion-list}
    :apropos   {:op/rpc "slynk-apropos:apropos-list-for-emacs"
                :op/args [:pattern :external-only :case-sensitive :package]
                :op/defaults {:external-only true :case-sensitive false :package nil}
                :op/shape :plist-list}
    :status    {:op/rpc "slynk:connection-info"
                :op/args [] :op/shape :plist}
    :load-file {:op/rpc "slynk:load-file"
                :op/args [:filename] :op/shape :string}
    :inspect   {:op/rpc "slynk:init-inspector"
                :op/args [:form] :op/shape :plist}
    :restart   {:op/rpc "slynk:invoke-nth-restart-for-emacs"
                :op/args [:level :n] :op/shape :any}}
   :profile/langs
   {:clojure {:lang/template
              (str "(let ((*readtable* (named-readtables:find-readtable 'cloture::cloture))"
                   "      (*package* (find-package \"user\")))"
                   "  (cloture::compile-and-eval (read-from-string %s)))")}}})

(def default-profiles
  "Profiles registered at load time, in registry order."
  [cider-profile slynk-profile])

;;; =============================================================================
;;; Registry
;;; =============================================================================

(defonce ^:private registry
  (atom (into {} (map (juxt :profile/id identity)) default-profiles)))

(m/=> register! [:=> [:cat schema/Profile] :keyword])

(defn register!
  "Register PROFILE under its id, replacing any profile sharing it. Returns the
   id. Throws when PROFILE does not conform to schema/Profile."
  [profile]
  (when-not (m/validate schema/Profile profile)
    (throw (ex-info "invalid backend profile"
                    {:explain (m/explain schema/Profile profile)})))
  (swap! registry assoc (:profile/id profile) profile)
  (:profile/id profile))

(m/=> profile [:=> [:cat :keyword] [:maybe schema/Profile]])

(defn profile
  "The profile registered under BACKEND-ID, or nil."
  [backend-id]
  (get @registry backend-id))

(m/=> backends [:=> [:cat] [:vector :keyword]])

(defn backends
  "Registered backend ids, sorted."
  []
  (vec (sort (keys @registry))))

(m/=> supports? [:=> [:cat :keyword :keyword] :boolean])

(defn supports?
  "Does BACKEND-ID serve VERB?"
  [backend-id verb]
  (boolean (get-in @registry [backend-id :profile/ops verb])))

(m/=> capabilities [:=> [:cat :keyword] [:vector :keyword]])

(defn capabilities
  "Verbs BACKEND-ID serves, sorted. Empty when the backend is unregistered."
  [backend-id]
  (vec (sort (keys (get-in @registry [backend-id :profile/ops] {})))))

(defn reset-registry!
  "Restore the registry to `default-profiles`. For fixtures."
  []
  (reset! registry (into {} (map (juxt :profile/id identity)) default-profiles))
  nil)
