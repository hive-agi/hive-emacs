(ns hive-emacs.editor.translators
  "hive-vessel translators lowering the editor probe ops to :elisp natives.

   A CLOSED probe vocabulary: each op asks one fixed question of the editor.
   There is deliberately no generic evaluate-this-string op (the vessel
   contract is 'never editor evaluation').

   Pure: op map in, native op out. The feature name is gated by
   :translator/accepts to the elisp-symbol charset the addon doctor already
   enforces, and is written through hive-vessel.dialect.elisp/symbol-literal.

   Ops:
     {:op :editor/feature? :feature \"hive-mcp\"}  ; answers \"t\" / \"nil\""
  (:require [hive-vessel.dialect.elisp :as el]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def FeatureName
  "An Emacs feature symbol, without the leading quote. The same charset the
   host's addon doctor accepts for its feature expectations. Anchored with
   \\z, not $: malli :re is a re-find and Java's $ also matches before a
   final line terminator, which would let \"foo\\n\" through."
  [:and
   [:string {:min 1 :max 256}]
   [:re #"^[A-Za-z0-9][A-Za-z0-9+*./_:@~-]*\z"]])

(defn feature-code
  "(featurep 'FEATURE): is FEATURE loaded in this Emacs."
  [{:keys [feature]}]
  (str "(featurep '" (el/symbol-literal feature) ")"))

(defn- lowering
  "A translator lowering OP through CODE-FN, gated by ACCEPTS."
  [op code-fn accepts]
  {:translator/id (keyword "hive-emacs" (str (namespace op) "-" (name op)))
   :translator/op op
   :translator/when {:vessel/dialect el/dialect}
   :translator/translate (fn [o _target] (el/native (code-fn o)))
   :translator/accepts accepts})

(def translators
  "Every editor probe translator, contributed under the IAddon hook
   hive-vessel.core/hook-key."
  [(lowering :editor/feature? feature-code
             [:map [:feature FeatureName]])])
