(ns hive-emacs.tools.docs
  "Emacs documentation handlers owned by hive-emacs."
  (:require [clojure.string :as str]
            [hive-dsl.result :as result]
            [hive-emacs.client :as ec]
            [hive-emacs.tools.support :as tool]))

(defn docs-addon-available?
  []
  (try
    (let [{:keys [success result]}
          (ec/eval-elisp "(featurep 'hive-mcp-docs)")]
      (and success (= "t" (str/trim (or result "")))))
    (catch Exception _ false)))

(defn- eval-docs
  [category expression]
  (tool/result->mcp-text
   (tool/try-result
    category
    #(if-not (docs-addon-available?)
       (result/err :docs/addon-not-loaded
                   {:message "hive-mcp-docs addon is not loaded"})
       (let [{:keys [success result error]} (ec/eval-elisp expression)]
         (if success
           (result/ok result)
           (result/err :docs/elisp-error
                       {:message (str "Elisp error: " error)})))))))

(defn- symbol-call
  [category function-name value]
  (if-not (seq value)
    (tool/mcp-error "symbol name is required")
    (eval-docs
     category
     (format "(json-encode (%s (intern %s)))"
             function-name (pr-str value)))))

(defn handle-describe-function
  [{:keys [function_name functionName]}]
  (symbol-call :docs/describe-function
               "hive-mcp-docs-describe-function"
               (or function_name functionName)))

(defn handle-describe-variable
  [{:keys [variable_name variableName]}]
  (symbol-call :docs/describe-variable
               "hive-mcp-docs-describe-variable"
               (or variable_name variableName)))

(defn handle-apropos
  [{:keys [pattern type]}]
  (if-not (seq pattern)
    (tool/mcp-error "pattern is required")
    (eval-docs
     :docs/apropos
     (if type
       (format "(json-encode (hive-mcp-docs-apropos %s %s))"
               (pr-str pattern) (pr-str type))
       (format "(json-encode (hive-mcp-docs-apropos %s))"
               (pr-str pattern))))))

(defn handle-package-functions
  [{:keys [package_or_prefix packageOrPrefix]}]
  (let [value (or package_or_prefix packageOrPrefix)]
    (if-not (seq value)
      (tool/mcp-error "package_or_prefix is required")
      (eval-docs
       :docs/package-functions
       (format "(json-encode (hive-mcp-docs-package-functions %s))"
               (pr-str value))))))

(defn handle-find-keybindings
  [{:keys [command]}]
  (symbol-call :docs/find-keybindings
               "hive-mcp-docs-find-keybindings"
               command))

(defn handle-package-commentary
  [{:keys [package_name packageName]}]
  (symbol-call :docs/package-commentary
               "hive-mcp-docs-package-commentary"
               (or package_name packageName)))

(defn handle-list-packages
  [_]
  (eval-docs :docs/list-packages
             "(json-encode (hive-mcp-docs-list-packages))"))
