(ns hive-emacs.bridge-loader
  "Dependency-free loader for the Emacs-side hive-mcp bridge."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def ^:private addons-marker "hive-mcp-addons.el")

(def ^:private load-lock
  (Object.))

(defn resolve-elisp-dirs
  "Return compiled bridge and runtime directories found on the classpath."
  []
  (cond-> []
    (io/resource addons-marker)
    (conj (-> (io/resource addons-marker)
              .getPath
              (str/replace (re-pattern (str "/" addons-marker "$")) "")))

    (io/resource "clojure-elisp/clojure-elisp-runtime.el")
    (conj (-> (io/resource "clojure-elisp/clojure-elisp-runtime.el")
              .getPath
              (str/replace #"/clojure-elisp-runtime\.el$" "")))))

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
  "Ensure the bridge is ready in Emacs.

   EVAL-FN accepts Elisp plus timeout milliseconds and returns a map with
   :success. Readiness is re-probed because Emacs can restart independently
   from the host JVM. Loading is serialized so concurrent addon initialization
   cannot race."
  [eval-fn]
  (locking load-lock
    (if (eval-success? (eval-fn (bridge-ready-elisp) 5000))
      true
      (let [dirs (resolve-elisp-dirs)]
        (boolean
         (and (seq dirs)
              (eval-success? (eval-fn (load-path-elisp dirs) 5000))
              (eval-success? (eval-fn (load-entrypoints-elisp) 15000))))))))