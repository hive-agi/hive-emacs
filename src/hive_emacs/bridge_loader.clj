(ns hive-emacs.bridge-loader
  "Dependency-free loader for the Emacs-side hive-mcp bridge."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def ^:private addons-marker "hive-mcp-addons.el")

(def ^:private runtime-marker "clojure-elisp/clojure-elisp-runtime.el")

(defn- jar-cache-dir
  "Cache directory for .el files extracted from JAR — keyed by jar name+mtime."
  ^java.io.File [^java.util.jar.JarFile jar]
  (let [f (io/file (.getName jar))]
    (io/file (System/getProperty "user.home")
             ".cache" "hive-emacs" "bridge"
             (str (.getName f) "-" (.lastModified f)))))

(defn- extract-jar-elisp!
  "Extract the sibling .el entries of URL's jar entry into the per-jar cache.
   Returns the cache directory's absolute path, or nil when nothing extracted."
  [^java.net.URL url]
  (let [conn ^java.net.JarURLConnection (.openConnection url)
        jar (.getJarFile conn)
        entry (.getEntryName conn)
        slash (.lastIndexOf entry "/")
        prefix (if (neg? slash) "" (subs entry 0 (inc slash)))
        cache (jar-cache-dir jar)]
    (doseq [^java.util.jar.JarEntry e (enumeration-seq (.entries jar))
            :let [n (.getName e)
                  rel (when (and (str/starts-with? n prefix)
                                 (str/ends-with? n ".el"))
                        (subs n (count prefix)))]
            :when (and rel (seq rel) (not (str/includes? rel "/")))]
      (let [out (io/file cache rel)]
        (when-not (.exists out)
          (io/make-parents out)
          (with-open [in (.getInputStream jar e)]
            (io/copy in out)))))
    (when (.exists cache) (.getAbsolutePath cache))))

(defn- resource->load-dir
  "Directory Emacs can add to load-path for a classpath resource URL.
   file: URLs resolve to the containing directory; jar: URLs extract their
   sibling .el entries into a per-jar cache directory."
  [^java.net.URL url]
  (case (.getProtocol url)
    "file" (.getParent (io/file (.getPath url)))
    "jar" (extract-jar-elisp! url)
    nil))

(def ^:private load-lock
  (Object.))

(defn resolve-elisp-dirs
  "Return bridge and runtime load-path directories found on the classpath.
   RESOURCE-FN maps a resource path to a URL; defaults to `io/resource`."
  ([] (resolve-elisp-dirs io/resource))
  ([resource-fn]
   (into []
         (keep #(some-> (resource-fn %) resource->load-dir))
         [addons-marker runtime-marker])))

(defn load-path-elisp
  "Build Elisp that adds directories to load-path."
  [dirs]
  (format "(progn %s t)"
          (str/join " "
                    (map #(format "(add-to-list 'load-path %s)" (pr-str %))
                         dirs))))

(defn bridge-ready-elisp
  "Build Elisp that fails unless the loaded bridge exposes its required API."
  []
  (str "(progn "
       "(unless (and "
       "(featurep 'hive-mcp) "
       "(featurep 'hive-mcp-cider) "
       "(fboundp 'hive-mcp-mode) "
       "(fboundp 'hive-mcp-cider-spawn-session) "
       "(fboundp 'hive-mcp-cider-eval-in-session) "
       "(fboundp 'hive-mcp-cider-list-sessions) "
       "(bound-and-true-p hive-mcp-mode)) "
       "(error \"hive-emacs bridge entrypoints unavailable\")) "
       "t)"))

(defn load-entrypoints-elisp
  "Build Elisp that requires and validates the bridge entrypoints."
  []
  (str "(progn "
       "(require 'hive-mcp) "
       "(require 'hive-mcp-cider) "
       "(unless (and "
       "(fboundp 'hive-mcp-mode) "
       "(fboundp 'hive-mcp-cider-spawn-session) "
       "(fboundp 'hive-mcp-cider-eval-in-session) "
       "(fboundp 'hive-mcp-cider-list-sessions)) "
       "(error \"hive-emacs bridge entrypoints unavailable\")) "
       "(hive-mcp-mode 1) "
       "(unless (bound-and-true-p hive-mcp-mode) "
       "(error \"hive-mcp-mode failed to enable\")) "
       "t)"))

(defn- eval-success?
  [result]
  (true? (:success result)))

(defn ensure-loaded!
  "Ensure required bridge entrypoints are ready in Emacs.

   Injects any classpath-resolved elisp dirs into load-path, then requires the
   entrypoints. An empty dir set is NOT fatal: Emacs may already carry the
   bridge on its own load-path, so the require is attempted regardless.

   EVAL-FN accepts Elisp plus timeout milliseconds and returns a map with
   :success."
  [eval-fn]
  (locking load-lock
    (if (eval-success? (eval-fn (bridge-ready-elisp) 5000))
      true
      (let [dirs (resolve-elisp-dirs)]
        (boolean
         (and (or (empty? dirs)
                  (eval-success? (eval-fn (load-path-elisp dirs) 5000)))
              (eval-success? (eval-fn (load-entrypoints-elisp) 15000))))))))

(defn eval-with-bridge
  "Ensure bridge entrypoints are loaded, then evaluate CODE with TIMEOUT-MS.
   Returns the evaluator result map."
  [eval-fn code timeout-ms]
  (if (ensure-loaded! eval-fn)
    (eval-fn code timeout-ms)
    {:success false
     :error "Emacs bridge entrypoints failed to load"
     :bridge-unavailable true}))
