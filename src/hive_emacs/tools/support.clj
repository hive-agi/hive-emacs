(ns hive-emacs.tools.support
  "Host-neutral MCP response and command-dispatch helpers."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [hive-dsl.result :as result]
            [hive-emacs.schema :as schema]
            [malli.core :as m]))

(defn mcp-success
  [value]
  (schema/validate!
   :hive-emacs/mcp-response
   {:type "text"
    :text (if (string? value) value (pr-str value))}))

(defn mcp-json
  [value]
  (schema/validate!
   :hive-emacs/mcp-response
   {:type "text" :text (json/write-str value)}))

(defn mcp-error
  [message]
  (schema/validate!
   :hive-emacs/mcp-response
   {:type "text" :text (str message) :isError true}))

(defn try-result
  "Run F, converting exceptions into a Result error."
  [category f]
  (result/bind (result/try-effect* category (f)) identity))

(defn- error-message
  [r]
  (str (or (:message r) (:error r))
       (when-let [class-name (:class r)]
         (str " (" class-name ")"))))

(defn result->mcp
  [r]
  (if (result/ok? r)
    (mcp-json (:ok r))
    (mcp-error (error-message r))))

(defn result->mcp-text
  [r]
  (if (result/ok? r)
    (mcp-success (:ok r))
    (mcp-error (error-message r))))

(defn- normalize-command
  [command]
  (cond
    (keyword? command) (name command)
    (string? command) command
    :else nil))

(defn- command-path
  [command]
  (let [command (normalize-command command)]
    (when-not (str/blank? command)
      (mapv keyword (str/split (str/trim command) #"\s+")))))

(defn- collect-command-paths
  [handlers prefix]
  (reduce-kv
   (fn [paths key handler]
     (cond
       (= key :_handler) paths
       (fn? handler) (conj paths (conj prefix key))
       (map? handler)
       (into paths
             (cond-> (collect-command-paths handler (conj prefix key))
               (contains? handler :_handler) (conj (conj prefix key))))
       :else paths))
   []
   handlers))

(defn format-help
  [handlers]
  (let [paths (sort-by #(str/join " " (map name %))
                       (collect-command-paths handlers []))]
    (str "Available commands:\n"
         (str/join "\n"
                   (map #(str "  - " (str/join " " (map name %))) paths)))))

(defn- resolve-handler
  [handlers path]
  (loop [node handlers
         remaining path]
    (if (empty? remaining)
      (cond
        (fn? node) node
        (map? node) (:_handler node)
        :else nil)
      (let [next-node (when (map? node) (get node (first remaining)))]
        (cond
          (fn? next-node) (when (= 1 (count remaining)) next-node)
          (map? next-node) (recur next-node (rest remaining))
          :else (when (map? node) (:_handler node)))))))

(defn make-cli-handler
  "Build a nested, CLI-style command handler without host middleware."
  [handlers]
  (fn [{:keys [command] :as params}]
    (let [path (command-path command)]
      (cond
        (= [:help] path) (mcp-success (format-help handlers))
        (nil? path) (mcp-error (str "Unknown command: " command))
        :else
        (if-let [handler (resolve-handler handlers path)]
          (handler params)
          (mcp-error (str "Unknown command: " command
                          ". Valid: " (keys handlers))))))))

(m/=> mcp-success [:=> [:cat :any] schema/McpResponse])
(m/=> mcp-json [:=> [:cat :any] schema/McpResponse])
(m/=> mcp-error [:=> [:cat :any] schema/McpResponse])
(m/=> try-result [:=> [:cat :qualified-keyword ifn?] schema/Result])
(m/=> result->mcp [:=> [:cat schema/Result] schema/McpResponse])
(m/=> result->mcp-text [:=> [:cat schema/Result] schema/McpResponse])
