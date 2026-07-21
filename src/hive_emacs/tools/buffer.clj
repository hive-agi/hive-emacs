(ns hive-emacs.tools.buffer
  "Emacs buffer and bridge handlers owned by hive-emacs."
  (:require [clojure.data.json :as json]
            [hive-dsl.result :as result]
            [hive-emacs.client :as ec]
            [hive-emacs.tools.support :as tool]))

(defn- required!
  [params key]
  (let [value (get params key)]
    (when (or (nil? value)
              (and (string? value) (empty? value)))
      (throw (ex-info (str (name key) " is required") {:field key})))
    value))

(defn- guarded-text
  [category f]
  (tool/result->mcp-text
   (tool/try-result category #(result/ok (f)))))

(defn- eval-text
  [category expression]
  (tool/result->mcp-text
   (tool/try-result
    category
    #(let [{:keys [success result error]} (ec/eval-elisp expression)]
       (if success
         (result/ok result)
         (result/err category {:message (str error)}))))))

(defn bridge-available?
  []
  (let [{:keys [success result]}
        (ec/eval-elisp "(featurep 'hive-mcp-api)")]
    (and success (= "t" result))))

(defn handle-goto-line
  [{:keys [line]}]
  (guarded-text
   :emacs/goto-line-failed
   #(do
      (when-not (and (integer? line) (pos? line))
        (throw (ex-info "line must be a positive integer" {:line line})))
      (ec/goto-line line)
      (str "Moved to line " line))))

(defn handle-insert-text
  [params]
  (let [text (required! params :text)]
    (guarded-text
     :emacs/insert-failed
     #(do (ec/insert-text text) "Text inserted"))))

(defn handle-project-root
  [_]
  (guarded-text :emacs/project-root-failed
                #(or (ec/project-root) "No project detected")))

(defn handle-recent-files
  [_]
  (guarded-text :emacs/recent-files-failed ec/recent-files))

(defn handle-context
  [_]
  (if (bridge-available?)
    (eval-text :emacs/context-failed
               "(json-encode (hive-mcp-api-get-context))")
    (tool/mcp-error "Emacs bridge API is not loaded")))

(defn handle-capabilities
  [_]
  (if (bridge-available?)
    (eval-text :emacs/capabilities-failed
               "(json-encode (hive-mcp-api-capabilities))")
    (tool/mcp-success
     (json/write-str {:available false
                      :message "Emacs bridge API is not loaded"}))))

(defn handle-workflows
  [_]
  (if (bridge-available?)
    (eval-text :emacs/workflows-failed
               "(json-encode (hive-mcp-api-list-workflows))")
    (tool/mcp-error "Emacs bridge API is not loaded")))

(defn handle-special-buffers
  [_]
  (eval-text
   :emacs/special-buffers-failed
   "(prin1-to-string (mapcar #'buffer-name (seq-filter (lambda (buf) (string-match-p \"^\\\\*\" (buffer-name buf))) (buffer-list))))"))

(defn handle-buffer-info
  [params]
  (let [buffer-name (or (:buffer_name params) (:buffer params))]
    (if-not (seq buffer-name)
      (tool/mcp-error "buffer_name is required")
      (eval-text
       :emacs/buffer-info-failed
       (format
        "(with-current-buffer %s (json-encode (list :name (buffer-name) :size (buffer-size) :lines (count-lines (point-min) (point-max)) :mode (symbol-name major-mode) :modified (buffer-modified-p) :file (buffer-file-name) :point (point) :point-max (point-max))))"
        (pr-str buffer-name))))))
