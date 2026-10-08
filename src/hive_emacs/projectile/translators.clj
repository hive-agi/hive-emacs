(ns hive-emacs.projectile.translators
  "Closed projectile operations; typed strings are dialect-escaped."
  (:require [hive-vessel.dialect.elisp :as el]
            [clojure.string :as str]))

(defn- call [f & args]
  (str "(" f (apply str (map #(str " " %) args)) ")"))
(defn- code [f arg]
  (call "progn" (call "require" "'hive-mcp-projectile" "nil" "t")
        (call "json-encode" (call f (if (some? arg) (el/string-literal arg) "")))))
(defn files-code [{:keys [pattern]}]
  (if pattern (code "hive-mcp-projectile-api-project-files" pattern)
      (call "progn" (call "require" "'hive-mcp-projectile" "nil" "t")
            (call "json-encode" (call "hive-mcp-projectile-api-project-files")))))
(defn find-code [{:keys [filename]}]
  (code "hive-mcp-projectile-api-find-file" filename))
(defn search-code [{:keys [pattern]}]
  (code "hive-mcp-projectile-api-search" pattern))
(defn recent-code [_]
  (call "progn" (call "require" "'hive-mcp-projectile" "nil" "t")
        (call "json-encode" (call "hive-mcp-projectile-api-recent-files"))))
(defn list-code [_]
  (call "progn" (call "require" "'hive-mcp-projectile" "nil" "t")
        (call "json-encode" (call "hive-mcp-projectile-api-list-projects"))))
(def ^:private NonBlank [:and [:string] [:fn (complement str/blank?)]])
(defn- translator [op f schema]
  {:translator/id (keyword "hive-emacs" (str "projectile-" (name op)))
   :translator/op op :translator/when {:vessel/dialect el/dialect}
   :translator/accepts schema
   :translator/translate (fn [op _target] (el/native (f op)))})
(def translators
  [(translator :project/files files-code [:map [:pattern {:optional true} [:maybe NonBlank]]])
   (translator :project/find-file find-code [:map [:filename NonBlank]])
   (translator :project/search search-code [:map [:pattern NonBlank]])
   (translator :project/recent recent-code [:map])
   (translator :project/list-projects list-code [:map])])
