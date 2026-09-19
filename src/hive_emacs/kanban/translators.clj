(ns hive-emacs.kanban.translators
  "hive-vessel translators lowering the kanban ops to :elisp
   natives for the Emacs swarm addon.

   Pure: op map in, native op out. Every literal goes through
   hive-vessel.dialect.elisp (string-literal / symbol-literal /
   data-literal), so interpolated values are escaped by the dialect.

   Ops:
     {:op :kanban/move-to-done :task-id id :directory dir-or-nil}"
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

(defn- json-call
  "(json-encode (F ARGS...))"
  [f & args]
  (form "json-encode" (apply form f args)))

(def ^:private s el/string-literal)

(def ^:private NonBlankStr
  "A string with at least one non-whitespace character."
  [:and [:string] [:fn (complement str/blank?)]])

(defn move-to-done-code
  "DIRECTORY is the api's own third argument (nil when absent), as the core
   effect passed it before the op existed."
  [{:keys [task-id directory]}]
  (json-call "hive-mcp-api-kanban-move"
             (s task-id)
             (s "done")
             (if directory (s directory) "nil")))

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
  "Every kanban translator, contributed under the IAddon hook
   hive-vessel.core/hook-key."
  [(lowering :kanban/move-to-done #(move-to-done-code %)
             [:map [:task-id NonBlankStr]
              [:directory {:optional true} NonBlankStr]])])
