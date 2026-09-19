(ns hive-emacs.project.translators
  "hive-vessel translators lowering the projectile/project ops to :elisp
   natives for the Emacs swarm addon.

   Pure: op map in, native op out. Every literal goes through
   hive-vessel.dialect.elisp (string-literal / symbol-literal /
   data-literal), so interpolated values are escaped by the dialect.

   Ops:
     {:op :project/info :directory dir-or-nil}"
  (:require [hive-vessel.dialect.elisp :as el]
            [clojure.string :as str]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defn- form
  "Elisp call source: function F (a name) applied to ARGS, each already an
   elisp source fragment."
  [f & args]
  (apply str "(" (el/symbol-literal f) (concat (map #(str " " %) args) [")"])))

(defn- quoted-symbol
  "Elisp source for the quoted symbol named NAME."
  [name]
  (str "'" (el/symbol-literal name)))

(defn- json-call
  "(json-encode (F ARGS...))"
  [f & args]
  (form "json-encode" (apply form f args)))

(def ^:private s el/string-literal)

(def ^:private NonBlankStr
  "A string with at least one non-whitespace character."
  [:and [:string] [:fn (complement str/blank?)]])

(defn- let-bindings
  "Elisp let-bindings source for a single VAR/VALUE pair, each an elisp
   source fragment: ((var value))."
  [var value]
  (str "(" (form var value) ")"))

(defn info-code [{:keys [directory]}]
  (let [body (form "progn"
                   (form "require" (quoted-symbol "hive-mcp-projectile"))
                   (json-call "hive-mcp-projectile-api-project-info"))]
    (if directory
      (form "let"
            (let-bindings "default-directory" (s (str directory "/")))
            body)
      body)))

(def ^:private Str [:string])

(defn- lowering
  "A translator lowering OP through CODE-FN, gated by ACCEPTS when given."
  ([op code-fn] (lowering op code-fn nil))
  ([op code-fn accepts]
   (cond-> {:translator/id (keyword "hive-emacs" (str (namespace op) "-" (name op)))
            :translator/op op
            :translator/when {:vessel/dialect el/dialect}
            :translator/translate (fn [o _target] (el/native (code-fn o)))}
     accepts (assoc :translator/accepts accepts))))

(def translators
  "Every project translator, contributed under the IAddon hook
   hive-vessel.core/hook-key."
  [(lowering :project/info #(info-code %)
             [:map [:directory {:optional true} NonBlankStr]])])
