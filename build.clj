(ns build
  "hive-emacs library build: thin jar + Clojars deploy.

   Version source of truth is the top-level VERSION file — the same value the
   GitHub release.yml tags as v{VERSION}, so the Clojars coord and the git-tag
   coord match 1:1. Never hardcode it here.

   Tasks (invoke with `clojure -T:build <task>`):
     clean   delete target/
     jar     build the thin jar + pom under target/
     deploy  jar + push to Clojars (needs CLOJARS_USERNAME / CLOJARS_PASSWORD)"
  (:require [clojure.tools.build.api :as b]
            [clojure.string :as str]))

(def lib 'io.github.hive-agi/hive-emacs)
(def version (str/trim (slurp "VERSION")))
(def class-dir "target/classes")
(def jar-file (format "target/%s-%s.jar" (name lib) version))
(def basis (delay (b/create-basis {:project "deps.edn"})))

;; Mirrors :paths in deps.edn — the elisp/ tree ships in the jar because the
;; Emacs vessel loads its .el helpers off the classpath at runtime.
(def src-dirs ["src" "resources" "elisp"])

(def pom-data
  [[:description "Emacs vessel addon for hive-mcp — implements IVessel/IEditor for Emacs (emacsclient subprocess with circuit breaker, multi-daemon DataScript store, elisp helpers, Olympus grid layout)."]
   [:url "https://github.com/hive-agi/hive-emacs"]
   ;; Matches LICENSE: EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0
   [:licenses
    [:license
     [:name "MIT"]
     [:url "https://opensource.org/license/mit"]]]
   [:scm
    [:url "https://github.com/hive-agi/hive-emacs"]
    [:connection "scm:git:git://github.com/hive-agi/hive-emacs.git"]
    [:developerConnection "scm:git:ssh://git@github.com/hive-agi/hive-emacs.git"]
    [:tag (str "v" version)]]
   [:developers
    [:developer
     [:name "Pedro G. Branquinho"]]]])

(defn clean [_]
  (b/delete {:path "target"}))

(defn jar
  "Build the library thin jar + pom for Clojars/Maven consumption."
  [_]
  (clean nil)
  (b/write-pom {:class-dir class-dir
                :lib lib
                :version version
                :basis @basis
                :src-dirs src-dirs
                :pom-data pom-data})
  (b/copy-dir {:src-dirs src-dirs
               :target-dir class-dir})
  (b/jar {:class-dir class-dir
          :jar-file jar-file})
  (println (str "Built " jar-file)))

(defn deploy
  "Deploy the library jar to Clojars. Requires CLOJARS_USERNAME + CLOJARS_PASSWORD
   (a Clojars deploy token) in the environment."
  [_]
  (jar nil)
  ((requiring-resolve 'deps-deploy.deps-deploy/deploy)
   {:installer :remote
    :artifact  jar-file
    :pom-file  (b/pom-path {:lib lib :class-dir class-dir})})
  (println (str "Deployed " lib " " version " to Clojars")))
