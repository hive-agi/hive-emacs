(ns hive-emacs.memory.translators
  "Closed legacy memory export: four fixed types, bounded to 1000 each."
  (:require [hive-vessel.dialect.elisp :as el]
            [clojure.string :as str]))

(defn legacy-export-code [{:keys [project-id]}]
  (let [pid (el/string-literal project-id)
        query (fn [type]
                (str "(hive-mcp-memory-query '" type " nil " pid " 1000 nil t)"))]
    (str "(json-encode (list :notes " (query "note")
         " :snippets " (query "snippet")
         " :conventions " (query "convention")
         " :decisions " (query "decision") "))")))

(def translators
  [{:translator/id :hive-emacs/memory-legacy-export
    :translator/op :memory/legacy-export
    :translator/when {:vessel/dialect el/dialect}
    :translator/accepts [:map [:project-id [:and [:string] [:fn (complement str/blank?)]]]]
    :translator/translate (fn [op _target] (el/native (legacy-export-code op)))}])
