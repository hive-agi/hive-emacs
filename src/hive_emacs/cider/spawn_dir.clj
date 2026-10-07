(ns hive-emacs.cider.spawn-dir
  "Which directory a `cider spawn` runs in."
  (:require [clojure.string :as str]
            [hive-dsl.result :as result]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defn- given [v]
  (when (and (string? v) (not (str/blank? v))) v))

(defn directory?
  "True when PATH names an existing directory."
  [path]
  (.isDirectory (java.io.File. ^String path)))

(def ^:dynamic *directory?*
  "The directory check resolve-dir uses when none is passed; read per call."
  directory?)

(defn resolve-dir
  "The directory a spawn runs in: project_dir, else directory, else nil (Emacs
   decides). Result: ok path-or-nil, or err :cider/bad-project-dir when the
   named path fails DIR? (default *directory?*)."
  ([params] (resolve-dir params *directory?*))
  ([{:keys [project_dir directory]} dir?]
   (let [p (or (given project_dir) (given directory))]
     (cond
       (nil? p) (result/ok nil)
       (dir? p) (result/ok p)
       :else    (result/err :cider/bad-project-dir
                            {:message (str "Error: spawn project_dir/directory is not an existing directory: " p)})))))
