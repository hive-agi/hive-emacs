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

(defn- already-published-private?
  "True if this exact lib+version pom is already in the private Gitea registry.
   Authed HEAD — private-org reads require credentials."
  [url username token]
  (let [[grp art] (str/split (str lib) #"/")
        pom-url (format "%s/%s/%s/%s/%s-%s.pom"
                        url (str/replace grp "." "/") art version art version)
        auth (str "Basic " (.encodeToString (java.util.Base64/getEncoder)
                                            (.getBytes (str username ":" token))))]
    (try
      (let [conn (doto ^java.net.HttpURLConnection (.openConnection (java.net.URL. pom-url))
                   (.setRequestMethod "HEAD")
                   (.setRequestProperty "Authorization" auth)
                   (.setConnectTimeout 10000)
                   (.setReadTimeout 10000))]
        (= 200 (.getResponseCode conn)))
      (catch Throwable _ false))))

(defn deploy-private
  "Build + deploy to the private Gitea Maven registry (hive-agi org).
   Env: GITEA_MAVEN_TOKEN (required, non-blank), GITEA_MAVEN_USERNAME (default buddhilw),
   GITEA_MAVEN_URL (default https://gitea.hive-mcp.com/api/packages/hive-agi/maven).
   No-ops when this version already exists in the registry (idempotent)."
  [_]
  (let [env (fn [k fallback]
              (let [v (System/getenv k)]
                (if (str/blank? v) fallback v)))
        url (env "GITEA_MAVEN_URL" "https://gitea.hive-mcp.com/api/packages/hive-agi/maven")
        username (env "GITEA_MAVEN_USERNAME" "buddhilw")
        token (System/getenv "GITEA_MAVEN_TOKEN")]
    (when (str/blank? token)
      (throw (ex-info "GITEA_MAVEN_TOKEN is required (non-blank)" {:env "GITEA_MAVEN_TOKEN"})))
    (if (already-published-private? url username token)
      (println "Skip:" (str lib) version "already in private registry — bump VERSION to release.")
      (do
        (jar nil)
        ((requiring-resolve 'deps-deploy.deps-deploy/deploy)
         {:installer  :remote
          :artifact   jar-file
          :pom-file   (b/pom-path {:lib lib :class-dir class-dir})
          :repository {"gitea" {:url url :username username :password token}}})
        (println "Deployed" (str lib) version "to" url)))))
