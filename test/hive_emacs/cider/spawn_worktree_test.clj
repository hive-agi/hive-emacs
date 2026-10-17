(ns hive-emacs.cider.spawn-worktree-test
  "A `cider spawn` given a LINKED git worktree runs in that worktree, never in
   the main checkout the worktree belongs to (tool friction
   20261017170125-3a91c14e). The fixture is a real temp git repo plus
   `git worktree add`; the two checkouts declare different deps.edn aliases,
   so a spawn planned against the main checkout is visible in both the
   directory and the composed -M aliases. The elisp boundary is a recording
   *eval-fn*: nothing is spawned."
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-emacs.cider.alias-compose :as alias-compose]
            [hive-emacs.cider.spawn-dir :as spawn-dir]
            [hive-emacs.tools.cider :as cider]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def ^:dynamic *main* "The main checkout's path." nil)
(def ^:dynamic *wt* "The linked worktree's path." nil)

(defn- git! [dir & args]
  (let [{:keys [exit err] :as r}
        (apply sh/sh "git" "-c" "user.name=t" "-c" "user.email=t@t"
               "-c" "init.defaultBranch=main" "-c" "commit.gpgsign=false"
               (concat args [:dir dir]))]
    (when-not (zero? exit)
      (throw (ex-info (str "git " (str/join " " args) " failed: " err) r)))
    (str/trim (:out r))))

(defn- delete-tree! [^java.io.File f]
  (when (.isDirectory f)
    (doseq [c (.listFiles f)] (delete-tree! c)))
  (.delete f))

(defn- with-linked-worktree [t]
  (let [root (.getCanonicalFile
              (.toFile (java.nio.file.Files/createTempDirectory
                        "spawn-wt" (make-array java.nio.file.attribute.FileAttribute 0))))
        main (io/file root "main")
        wt   (io/file root "wt")]
    (try
      (.mkdirs main)
      (git! (str main) "init" "-q")
      (spit (io/file main "deps.edn") (pr-str {:paths ["src"] :aliases {:dev {:extra-paths ["dev"]}}}))
      (git! (str main) "add" "deps.edn")
      (git! (str main) "commit" "-q" "-m" "main")
      (git! (str main) "worktree" "add" "-q" "-b" "feature" (str wt))
      ;; The worktree's own deps.edn declares :test too; the main checkout's does not.
      (spit (io/file wt "deps.edn")
            (pr-str {:paths ["src"] :aliases {:dev {:extra-paths ["dev"]} :test {:extra-paths ["test"]}}}))
      (binding [*main* (str main) *wt* (str wt)]
        (t))
      (finally
        (delete-tree! root)))))

(use-fixtures :each with-linked-worktree)

(defn- recording-eval [calls]
  (fn
    ([code] (swap! calls conj code) {:success true :result "\"{}\""})
    ([code _timeout-ms] (swap! calls conj code) {:success true :result "\"{}\""})))

(deftest fixture-is-a-linked-worktree-of-main
  (testing "git ties the worktree to the main checkout, so a resolver that
            followed --git-common-dir would land in main"
    (is (= *wt* (git! *wt* "rev-parse" "--show-toplevel")))
    (is (= (str *main* "/.git")
           (.getCanonicalPath
            (io/file (git! *wt* "rev-parse" "--path-format=absolute" "--git-common-dir")))))))

(deftest resolve-dir-keeps-the-worktree-path
  (is (= *wt* (:ok (spawn-dir/resolve-dir {:directory *wt*}))))
  (is (= *wt* (:ok (spawn-dir/resolve-dir {:project_dir *wt*})))))

(deftest aliases-come-from-the-worktree-deps-edn
  (is (= ["dev" "test"] (alias-compose/for-project *wt* nil "clj")))
  (is (= ["dev"] (alias-compose/for-project *main* nil "clj"))
      "the main checkout differs, so this assertion discriminates"))

(deftest spawn-plans-the-worktree-not-the-main-checkout
  (doseq [k [:directory :project_dir]]
    (let [calls (atom [])]
      (binding [cider/*eval-fn* (recording-eval calls)]
        (cider/handle-spawn {:name "wt-spawn" k *wt*}))
      (let [elisp (first @calls)]
        (is (some? elisp) (str k ": Emacs was asked to spawn"))
        (is (str/includes? elisp (str ":project-dir " (pr-str *wt*))) elisp)
        (is (not (str/includes? elisp (pr-str *main*)))
            (str k ": the main checkout's path never reaches Emacs"))
        (is (str/includes? elisp ":aliases '(\"dev\" \"test\")")
            (str k ": -M aliases are the worktree's deps.edn, not main's: " elisp))))))
