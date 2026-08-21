(ns hive-emacs.dev.addon-gate-audit
  "Audit every hive addon for a capability registration gated on a readiness
   probe — the defect fixed in hive-emacs cd66873.

   Reports each conditional whose TEST is a readiness probe (directly, or via a
   let-bound symbol holding one) and whose BODY registers a capability. Parsing
   is rewrite-clj node-level: no sexpr, no eval, no ns resolution.

   The two vocabularies are NAMED PATTERNS registered with hive-system.pattern,
   so this ns owns no regex of its own and asks its questions through the
   IPatternEngine port. Swapping in a structural engine changes nothing here.

   Entry points: (report), (audit), (audit-repos), (self-test)."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [hive-system.pattern.core :as pattern]
            [rewrite-clj.node :as n]
            [rewrite-clj.parser :as p]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;;; =============================================================================
;;; The vocabularies — data, registered once, referenced by id
;;; =============================================================================

(def audit-patterns
  "The two questions this audit asks of a source fragment."
  [#:pattern{:id    :audit/readiness-probe
             :expr  (str "available\\?|healthy\\?|reachable\\?|connected\\?"
                         "|running\\?|ready\\?|alive\\?|up\\?|responsive\\?"
                         "|ping|probe|health|loaded\\?|ensure-loaded|liveness"
                         "|can-connect|enabled\\?|configured\\?|emacs-running"
                         "|online\\?")
             :flags #{:case-insensitive}
             :doc   "Names that answer \"is the collaborator up right now?\""}
   #:pattern{:id   :audit/capability-registration
             :expr (str "contribute!|contribute-commands|register!|register-tool"
                        "|register-schema|register-many|register-port|set-port!"
                        "|set-store!|register-provider|register-backend"
                        "|register-source|register-strategy|add-tool|install!"
                        "|attach!")
             :doc  "Calls that publish a capability into a registry."}])

(defn register-patterns!
  "Register the audit vocabularies. Idempotent."
  []
  (mapv pattern/register-pattern! audit-patterns))

(defn- hit?
  "Does the pattern named by REF occur in TEXT?"
  [ref text]
  (true? (:ok (pattern/match? ref text))))

;;; =============================================================================
;;; Collect — manifests on disk
;;; =============================================================================

(def hive-root "/home/leibniz/PP/hive")

(def extra-roots
  "Addon repos that live outside hive-root."
  ["/home/leibniz/PP/vtranslate/hive-vtranslate"])

(defn- manifests-under
  [^java.io.File repo]
  (let [d (io/file repo "resources/META-INF/hive-addons")]
    (when (.isDirectory d)
      (->> (.listFiles d)
           (filter #(str/ends-with? (.getName ^java.io.File %) ".edn"))
           (map (fn [f] [repo f]))))))

(defn manifests
  "[{:addon/id :init-ns :repo}] for every addon manifest reachable on disk."
  []
  (->> (concat (mapcat manifests-under (filter #(.isDirectory ^java.io.File %)
                                               (.listFiles (io/file hive-root))))
               (mapcat (comp manifests-under io/file) extra-roots))
       (keep (fn [[repo ^java.io.File f]]
               (let [m (edn/read-string (slurp f))]
                 (when-let [init-ns (:addon/init-ns m)]
                   {:addon/id (:addon/id m) :init-ns init-ns :repo (str repo)}))))
       (sort-by :addon/id)
       vec))

;;; =============================================================================
;;; Promote — namespace to file
;;; =============================================================================

(defn ns->file
  "The source file backing INIT-NS inside REPO, or nil."
  [repo init-ns]
  (let [rel (-> init-ns (str/replace "-" "_") (str/replace "." "/"))]
    (->> (for [root ["src" "src/main" "src/clj" "."]
               ext  [".clj" ".cljc"]]
           (io/file repo root (str rel ext)))
         (filter #(.isFile ^java.io.File %))
         first)))

;;; =============================================================================
;;; Pipeline — the gate shape, over rewrite-clj nodes
;;; =============================================================================

(def conditionals
  #{"when" "if" "when-let" "if-let" "when-some" "if-some" "when-not" "if-not"})

(def binding-forms
  #{"let" "let*" "when-let" "if-let" "binding" "loop"})

(defn- children
  "Semantic children of NODE — whitespace and comments dropped."
  [node]
  (filter n/sexpr-able? (n/children node)))

(defn- nodes
  "Every node in NODE's tree, outermost first."
  [node]
  (tree-seq n/inner? n/children node))

(defn probe-aliases
  "Symbols bound to an expression that names a probe — the local names a gate
   hides behind (`bridge-ready?`)."
  [root]
  (into #{}
        (comp (filter #(= :list (n/tag %)))
              (filter #(binding-forms (some-> (first (children %)) n/string)))
              (mapcat (fn [node]
                        (let [v (second (children node))]
                          (when (and v (= :vector (n/tag v)))
                            (->> (children v)
                                 (partition 2)
                                 (keep (fn [[sym val]]
                                         (when (hit? :audit/readiness-probe (n/string val))
                                           (n/string sym)))))))))
              (remove #{"_"}))
        (nodes root)))

(defn gate-hit
  "A conditional whose test is a probe (direct or aliased) and whose branches
   register a capability. Returns a hit map or nil."
  [aliases node]
  (when (= :list (n/tag node))
    (let [[head test & branches] (children node)]
      (when (and head test (seq branches) (conditionals (n/string head)))
        (let [test-s (n/string test)
              body-s (str/join " " (map n/string branches))]
          (when (and (or (hit? :audit/readiness-probe test-s) (aliases test-s))
                     (hit? :audit/capability-registration body-s))
            {:test test-s
             :form (let [s (n/string node)]
                     (if (> (count s) 400) (str (subs s 0 400) " …") s))}))))))

(defn scan-source
  "Every gate hit in SOURCE."
  [source]
  (let [root    (p/parse-string-all source)
        aliases (probe-aliases root)]
    (into [] (keep #(gate-hit aliases %)) (nodes root))))

;;; =============================================================================
;;; Boundary
;;; =============================================================================

(defn- source-files
  [repo]
  (->> (io/file repo "src")
       file-seq
       (filter #(.isFile ^java.io.File %))
       (filter #(re-matches #".*\.cljc?" (.getName ^java.io.File %)))))

(defn audit
  "Scan every addon's INIT namespace. One row per addon."
  []
  (register-patterns!)
  (mapv (fn [{:keys [init-ns repo] :as m}]
          (if-let [f (ns->file repo init-ns)]
            (assoc m :file (str f) :hits (scan-source (slurp f)))
            (assoc m :file nil :hits [] :error :file-not-found)))
        (manifests)))

(defn audit-repos
  "Scan EVERY source file of each addon repo (plus the host) — a gate can sit
   one call deep from init. Rows carrying hits only."
  ([] (audit-repos (into (sorted-set (str (io/file hive-root "hive-mcp")))
                         (map :repo)
                         (manifests))))
  ([repos]
   (register-patterns!)
   (->> repos
        (mapcat (fn [repo]
                  (keep (fn [^java.io.File f]
                          (let [hits (try (scan-source (slurp f))
                                          (catch Exception e
                                            [{:test :parse-error :form (ex-message e)}]))]
                            (when (seq hits) {:repo repo :file (str f) :hits hits})))
                        (source-files repo))))
        vec)))

(defn self-test
  "The audit is worthless if it cannot see the instance it was written for:
   hive-emacs before cd66873."
  ([] (self-test hive-root "hive-emacs" "6e1ec6b:src/hive_emacs/addon.clj"))
  ([root repo rev-path]
   (register-patterns!)
   (let [{:keys [exit out]} (shell/with-sh-dir (str (io/file root repo))
                              (shell/sh "git" "show" rev-path))]
     (if (zero? exit)
       (let [hits (scan-source out)]
         {:detected? (boolean (seq hits)) :hits hits})
       {:detected? false :error :git-show-failed}))))

(defn report
  "Printed audit: self-test verdict first, then one line per addon, then the
   flagged forms in full."
  []
  (let [st   (self-test)
        rows (audit)]
    (println "self-test (pre-fix hive-emacs 6e1ec6b):"
             (if (:detected? st) "DETECTED" (str "MISSED " (:error st))))
    (doseq [h (:hits st)] (println "   " (:form h)))
    (println)
    (doseq [{:keys [hits file error] :as r} (sort-by (comp - count :hits) rows)]
      (println (format "%-24s %-14s %s"
                       (:addon/id r)
                       (if error (name error) (str (count hits) " hit(s)"))
                       (some-> file (str/replace hive-root "")))))
    (doseq [{:keys [hits] :as r} rows :when (seq hits)]
      (println)
      (println "====" (:addon/id r) "----" (:file r))
      (doseq [h hits]
        (println "  test:" (:test h))
        (println "  " (str/replace (:form h) "\n" "\n  "))))
    {:addons (count rows)
     :flagged (->> rows (filter (comp seq :hits)) (mapv :addon/id))
     :self-test-detected? (:detected? st)}))
