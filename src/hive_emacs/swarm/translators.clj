(ns hive-emacs.swarm.translators
  "hive-vessel translators lowering the swarm and CIDER-session ops to
   :elisp natives for the Emacs swarm addon.

   Pure: op map in, native op out. Every literal goes through
   hive-vessel.dialect.elisp (string-literal / symbol-literal /
   data-literal), so interpolated values are escaped by the dialect.

   Ops:
     {:op :swarm/available?}
     {:op :swarm/list-lings}
     {:op :swarm/status :slave-id id-or-nil}
     {:op :swarm/broadcast :prompt p}
     {:op :swarm/list-presets}
     {:op :swarm/pending-prompts}
     {:op :swarm/respond-prompt :slave-id s :response r}
     {:op :swarm/kill :slave-id s}
     {:op :swarm/send-prompt :slave-id s :prompt p}
     {:op :swarm/slave-ready? :slave-id s}
     {:op :swarm/collect :task-id t :timeout-ms n-or-nil}
     {:op :cider/spawn-session :name n}
     {:op :cider/kill-session :name n}"
  (:require [clojure.string :as str]
            [hive-vessel.dialect.elisp :as el]))
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

(defn available?-code [_op]
  (form "featurep" (quoted-symbol "hive-mcp-swarm")))

(defn list-lings-code [_op]
  (json-call "hive-mcp-swarm-list-lings"))

(defn status-code [{:keys [slave-id]}]
  (if slave-id
    (json-call "hive-mcp-swarm-status" (s slave-id))
    (json-call "hive-mcp-swarm-api-status")))

(defn broadcast-code [{:keys [prompt]}]
  (json-call "hive-mcp-swarm-broadcast" (s prompt)))

(defn list-presets-code [_op]
  (json-call "hive-mcp-swarm-api-list-presets"))

(defn pending-prompts-code [_op]
  (json-call "hive-mcp-swarm-api-pending-prompts"))

(defn respond-prompt-code [{:keys [slave-id response]}]
  (json-call "hive-mcp-swarm-api-respond-prompt" (s slave-id) (s response)))

(defn kill-code [{:keys [slave-id]}]
  (form "when"
        (form "fboundp" (quoted-symbol "hive-mcp-swarm-slaves-kill"))
        (form "hive-mcp-swarm-slaves-kill" (s slave-id))))

(defn send-prompt-code [{:keys [slave-id prompt]}]
  (form "hive-mcp-swarm-send-to-terminal" (s slave-id) (s prompt)))

(defn slave-ready?-code [{:keys [slave-id]}]
  (form "if"
        (form "hive-mcp-swarm-tasks--slave-ready-p" (s slave-id))
        (s "t")
        (s "nil")))

(defn collect-code [{:keys [task-id timeout-ms]}]
  (json-call "hive-mcp-swarm-api-collect" (s task-id) (el/data-literal timeout-ms)))

(defn spawn-session-code [{:keys [name]}]
  (json-call "hive-mcp-cider-spawn-session" (s name) "nil" "nil"))

(defn kill-session-code [{:keys [name]}]
  (form "hive-mcp-cider-kill-session" (s name)))

(def ^:private Str [:string])

(def ^:private NonBlankStr
  "A string with at least one non-whitespace character."
  [:and Str [:fn (complement str/blank?)]])

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
  "Every swarm / CIDER-session translator, contributed under the IAddon hook
   hive-vessel.core/hook-key."
  [(lowering :swarm/available? #(available?-code %))
   (lowering :swarm/list-lings #(list-lings-code %))
   (lowering :swarm/status #(status-code %)
             [:map [:slave-id {:optional true} [:maybe Str]]])
   (lowering :swarm/broadcast #(broadcast-code %)
             [:map [:prompt Str]])
   (lowering :swarm/list-presets #(list-presets-code %))
   (lowering :swarm/pending-prompts #(pending-prompts-code %))
   (lowering :swarm/respond-prompt #(respond-prompt-code %)
             [:map [:slave-id Str] [:response Str]])
   (lowering :swarm/kill #(kill-code %)
             [:map [:slave-id Str]])
   (lowering :swarm/send-prompt #(send-prompt-code %)
             [:map [:slave-id Str] [:prompt Str]])
   (lowering :swarm/slave-ready? #(slave-ready?-code %)
             [:map [:slave-id NonBlankStr]])
   (lowering :swarm/collect #(collect-code %)
             [:map [:task-id Str] [:timeout-ms {:optional true} [:maybe int?]]])
   (lowering :cider/spawn-session #(spawn-session-code %)
             [:map [:name Str]])
   (lowering :cider/kill-session #(kill-session-code %)
             [:map [:name Str]])])
