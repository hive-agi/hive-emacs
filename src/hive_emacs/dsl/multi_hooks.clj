(ns hive-emacs.dsl.multi-hooks
  "`:multi/*` IAddon hook entries owned by `hive.emacs`.

   Routed by hive-mcp `addons/core.clj` namespace dispatch into
   `hive-mcp.multi.registry` under owner `\"hive.emacs\"`. Adds compact
   verb codes for the consolidated `emacs` tool. The handler currently
   lives at `hive-mcp.tools.consolidated.emacs` (extraction boundary
   per `hive-emacs.addon` docstring); when it moves the verbs travel
   with the addon.

   Param aliases are owned by `hive.knowledge` to avoid first-write-wins
   conflicts at registration time.

   Decision: 20260429230453-7e7627cc.")

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def emacs-verbs
  "emacs — em-family (2-char prefix; `e` taken by enrich)."
  [{:code "em!" :tool "emacs" :command "eval"}
   {:code "em?" :tool "emacs" :command "status"}
   {:code "em*" :tool "emacs" :command "buffers"}
   {:code "em@" :tool "emacs" :command "current"}
   {:code "em>" :tool "emacs" :command "switch"}
   {:code "em/" :tool "emacs" :command "find"}
   {:code "em." :tool "emacs" :command "save"}
   {:code "em+" :tool "emacs" :command "insert"}
   {:code "emn" :tool "emacs" :command "notify"}
   {:code "emg" :tool "emacs" :command "goto-line"}
   {:code "emp" :tool "emacs" :command "project-root"}
   {:code "emr" :tool "emacs" :command "recent"}
   {:code "emc" :tool "emacs" :command "context"}
   {:code "emw" :tool "emacs" :command "workflows"}])

(def contributions
  "Map returned by the addon's `(hooks [this])` method.

   Vector values per `:multi/*` key match the
   `hive-mcp.multi.types/RegistryEntry` ADT shape."
  {:multi/verb emacs-verbs})
