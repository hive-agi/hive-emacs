(ns hive-emacs.tools.list-param
  "Reading an MCP list param that may arrive as TEXT.

   A client that was not shown a param's array schema (compact tools/list
   mode advertises only a tool's shared params, so an addon-contributed
   `aliases` is invisible to it) sends the array as its JSON TEXT: aliases
   [\"dev\"] arrives as the string \"[\\\"dev\\\"]\". Pure."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [hive-dsl.result :as result]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defn json-array-text
  "The entries of S when S is the JSON text of an array, else nil. Never throws."
  [s]
  (when (string? s)
    (let [t (str/trim s)]
      (when (str/starts-with? t "[")
        (let [parsed (result/rescue nil (json/read-str t))]
          (when (sequential? parsed) parsed))))))
