(ns hive-emacs.lsp-bridge
  "The Emacs strategy for lsp-mcp's live LSP bridge: lsp-mode answers through
   elisp/hive-lsp-bridge.el.

   lsp-mcp names no vessel. This ns registers the strategy into
   lsp-mcp.bridge when lsp-mcp is on the classpath, by soft resolution, so
   neither library compiles against the other."
  (:require [clojure.data.json :as json]
            [hive-emacs.bridge-loader :as bridge]
            [hive-emacs.client :as ec]
            [hive-spi.editor.elisp :as el]))

;; SPDX-License-Identifier: MIT

(def bridge-key
  "The key this strategy is registered under in lsp-mcp.bridge."
  :emacs)

(defn lsp-call
  "Call hive-lsp-bridge-<method> in Emacs with ARGS through EVAL-FN
   ((f code timeout-ms) -> {:success bool :result str}). Returns the parsed
   JSON answer, or {:error ...}."
  [eval-fn method & args]
  (let [code (apply el/require-and-call-json 'hive-lsp-bridge
                    (symbol (str "hive-lsp-bridge-" method)) args)
        res  (if (bridge/ensure-loaded-once! eval-fn)
               (eval-fn code 10000)
               (bridge/bridge-unavailable))]
    (if (:success res)
      (try
        (json/read-str (str (:result res)) :key-fn keyword)
        (catch Exception e
          {:error (str "JSON parse error: " (ex-message e)) :raw (:result res)}))
      {:error (or (:error res) "Emacs did not answer")})))

(defn- soft
  [sym]
  (try (requiring-resolve sym) (catch Exception _ nil)))

(defn register!
  "Register the Emacs live LSP bridge with lsp-mcp. Returns true when
   registered, false when lsp-mcp is not on the classpath."
  ([] (register! ec/eval-elisp-with-timeout))
  ([eval-fn]
   (let [reg (soft 'lsp-mcp.bridge/register-bridge!)
         mk  (soft 'lsp-mcp.bridge/fn-bridge)]
     (if (and reg mk)
       (do (reg bridge-key (mk {:available? (constantly true)
                                :call       (partial lsp-call eval-fn)}))
           true)
       false))))

(defn unregister!
  "Remove the Emacs live LSP bridge from lsp-mcp. No-op when absent."
  []
  (when-let [unreg (soft 'lsp-mcp.bridge/unregister-bridge!)]
    (unreg bridge-key))
  nil)
