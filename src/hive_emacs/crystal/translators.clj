(ns hive-emacs.crystal.translators
  "hive-vessel translators lowering the crystal harvest ops to :elisp
   natives for the Emacs swarm addon.

   Pure: op map in, native op out. Every literal goes through
   hive-vessel.dialect.elisp (string-literal / symbol-literal /
   data-literal), so interpolated values are escaped by the dialect.
   Quoted elisp symbols ('note, 'hive-mcp) are written as literal text,
   the same way project/translators.clj writes 'hive-mcp-projectile.

   Ops:
     {:op :crystal/available?}
     {:op :crystal/wrap-gather :directory dir-or-nil}
     {:op :crystal/session-notes :project-id pid-or-nil}
     {:op :crystal/kanban-notes :project-id pid-or-nil}
     {:op :crystal/git-commits :directory dir-or-nil}"
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

(def ^:private SinceStr
  "What may sit inside the single-quoted --since shell word: an ISO instant or a git date word."
  [:re #"^[0-9A-Za-z:.+\- ]{1,40}$"])

(defn- maybe-arg
  "VALUE as a literal when present, otherwise nil."
  [value]
  (if (some? value) (s value) "nil"))

(defn available-code
  "(featurep 'hive-mcp): is hive-mcp.el loaded in Emacs."
  [_op]
  (form "featurep" (quoted-symbol "hive-mcp")))

(defn wrap-gather-code
  "DIRECTORY is wrapped verbatim when present, exactly as the core's
   fetch-elisp-data builds it."
  [{:keys [directory]}]
  (if directory
    (json-call "hive-mcp-api-wrap-gather" (s directory))
    (json-call "hive-mcp-api-wrap-gather")))

(defn session-notes-code
  "PROJECT-ID is the memory query's scope (nil when absent); 50 notes of
   'ephemeral duration, as the core's session harvest builds it."
  [{:keys [project-id]}]
  (json-call "hive-mcp-memory-query"
             (quoted-symbol "note")
             "nil"
             (maybe-arg project-id)
             "50"
             (quoted-symbol "ephemeral")
             "nil"))

(defn kanban-notes-code
  "PROJECT-ID scopes both kanban-tagged queries; ephemeral and short-term
   notes appended, as the core's kanban harvest builds it."
  [{:keys [project-id]}]
  (let [q (fn [duration]
            (form "hive-mcp-memory-query"
                  (quoted-symbol "note")
                  "'(\"kanban\")"
                  (maybe-arg project-id)
                  "50"
                  (quoted-symbol duration)
                  "nil"))]
    (json-call "append" (q "ephemeral") (q "short-term"))))

(defn git-commits-code
  "DIRECTORY let-binds default-directory around the git log shell command
   when present, exactly as the core's commit harvest built it. SINCE is the
   session start the core recorded ('midnight' when it recorded none). It
   lands inside a single-quoted shell word, so :translator/accepts admits
   only the SinceStr charset."
  [{:keys [directory since]}]
  (let [cmd (str "git log --since='" (or since "midnight") "' --oneline 2>/dev/null")]
    (if directory
      (form "let"
            (str "(" (form "default-directory" (s directory)) ")")
            (form "shell-command-to-string" (s cmd)))
      (form "shell-command-to-string" (s cmd)))))

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
  "Every crystal translator, contributed under the IAddon hook
   hive-vessel.core/hook-key."
  [(lowering :crystal/available? available-code)
   (lowering :crystal/wrap-gather #(wrap-gather-code %)
             [:map [:directory {:optional true} NonBlankStr]])
   (lowering :crystal/session-notes #(session-notes-code %)
             [:map [:project-id {:optional true} NonBlankStr]])
   (lowering :crystal/kanban-notes #(kanban-notes-code %)
             [:map [:project-id {:optional true} NonBlankStr]])
   (lowering :crystal/git-commits #(git-commits-code %)
             [:map [:directory {:optional true} NonBlankStr]])])
