(ns hive-emacs.dependency-boundary-test
  "Executable architecture guard for the pure hive-addon boundary."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(def ^:private forbidden-host-api
  ;; ANY hive-mcp namespace, not a hand-listed subset: the leak that survived
  ;; the old (?:swarm|tools|protocols|addons|extensions|multi) list was
  ;; hive-mcp.agent.context, soft-resolved from the cider tool. The lookahead
  ;; spares "hive-mcp.el" and friends — those are elisp FILENAMES the bridge
  ;; loads, not host namespaces.
  #"hive-mcp\.(?!el\b)[a-z]")

(defn- clojure-sources
  []
  (->> (file-seq (io/file "src"))
       (filter #(.isFile ^java.io.File %))
       (filter #(str/ends-with? (.getName ^java.io.File %) ".clj"))))

(deftest production-code-has-no-host-api-reference
  (doseq [file (clojure-sources)]
    (is (not (re-find forbidden-host-api (slurp file)))
        (str "host API leaked into " (.getPath ^java.io.File file)))))

(deftest dependency-map-has-no-host-artifact
  (testing "hive-emacs depends on hive-addon abstraction, never a host core"
    (let [deps (edn/read-string (slurp "deps.edn"))
          artifact-names (->> deps :deps keys (map str) set)]
      (is (contains? artifact-names "io.github.hive-agi/hive-addon"))
      (is (not-any? #(str/includes? % "hive-mcp") artifact-names)))))
