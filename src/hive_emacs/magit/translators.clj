(ns hive-emacs.magit.translators
  "Closed magit operations translated into Emacs natives. No arbitrary eval operation."
  (:require [hive-vessel.dialect.elisp :as el]
            [clojure.string :as str]))

(defn- call [f & args]
  (str "(" f (apply str (map #(str " " %) args)) ")"))

(defn- maybe-string [x] (if (some? x) (el/string-literal x) "nil"))
(defn- required [code]
  (call "progn" (call "require" "'hive-mcp-magit" "nil" "t") code))
(defn- string-result [code]
  (required code))
(defn- json-result [code]
  (required (call "json-encode" code)))

(defn status-code [{:keys [directory]}]
  (json-result (call "hive-mcp-magit-api-status" (el/string-literal directory))))
(defn branches-code [{:keys [directory]}]
  (json-result (call "hive-mcp-magit-api-branches" (el/string-literal directory))))
(defn log-code [{:keys [directory count]}]
  (json-result (call "hive-mcp-magit-api-log" (str count) (el/string-literal directory))))
(defn diff-code [{:keys [directory target]}]
  (string-result (call "hive-mcp-magit-api-diff" (str "'" target) (el/string-literal directory))))
(defn stage-code [{:keys [files directory]}]
  (string-result (call "hive-mcp-magit-api-stage"
                       (if (= files :all) "'all" (str "'(" (str/join " " (map el/string-literal files)) ")"))
                       (el/string-literal directory))))
(defn commit-code [{:keys [message all directory]}]
  (string-result (call "hive-mcp-magit-api-commit" (el/string-literal message)
                       (if all "'(:all t)" "nil") (el/string-literal directory))))
(defn push-code [{:keys [set-upstream remote directory]}]
  (let [options (cond-> [] set-upstream (conj ":set-upstream t")
                        remote (conj (str ":remote " (el/string-literal remote))))]
    (string-result (call "hive-mcp-magit-api-push"
                         (if (seq options) (str "'(" (str/join " " options) ")") "nil")
                         (el/string-literal directory)))))
(defn pull-code [{:keys [directory]}]
  (string-result (call "hive-mcp-magit-api-pull" (el/string-literal directory))))
(defn fetch-code [{:keys [directory remote]}]
  (string-result (call "hive-mcp-magit-api-fetch" (maybe-string remote) (el/string-literal directory))))

(defn stage-verify-code [{:keys [directory paths]}]
  (let [ps (str "'(" (str/join " " (map el/string-literal paths)) ")")]
    (required
     (str "(let ((default-directory " (el/string-literal directory) ") (paths " ps ") (missing nil)) "
          "(dolist (p paths) (unless (file-exists-p p) (setq missing (or missing p)))) "
          "(json-encode (if missing (list :status \"missing\" :path missing) "
          "(progn (hive-mcp-magit-api-stage paths default-directory) "
          "(if (string-empty-p (string-trim (shell-command-to-string "
          "(concat \"git diff --cached --name-only -- \" "
          "(mapconcat #'shell-quote-argument paths \" \") \" 2>/dev/null\")))) "
          "(list :status \"empty\") (list :status \"ok\"))))))"))))

(defn feature-branches-code [{:keys [directory]}]
  (required
   (str "(let* ((default-directory " (el/string-literal directory) ") "
        "(branches (hive-mcp-magit-api-branches default-directory)) "
        "(local (plist-get branches :local)) "
        "(feature-branches (seq-filter (lambda (b) "
        "(or (string-prefix-p \"feature/\" b) (string-prefix-p \"fix/\" b) "
        "(string-prefix-p \"feat/\" b))) local))) "
        "(json-encode (list :current (plist-get branches :current) :feature_branches feature-branches)))")))

(def ^:private NonBlank [:and [:string] [:fn (complement str/blank?)]])
(def ^:private Base [:map [:directory NonBlank]])
(defn- translator [op f schema]
  {:translator/id (keyword "hive-emacs" (str "magit-" (name op)))
   :translator/op op
   :translator/when {:vessel/dialect el/dialect}
   :translator/accepts schema
   :translator/translate (fn [op _target] (el/native (f op)))})

(def translators
  [(translator :magit/status status-code Base)
   (translator :magit/branches branches-code Base)
   (translator :magit/log log-code [:map [:directory NonBlank] [:count [:int {:min 1 :max 10000}]]])
   (translator :magit/diff diff-code [:map [:directory NonBlank] [:target [:enum "staged" "unstaged" "all"]]])
   (translator :magit/stage stage-code [:map [:directory NonBlank] [:files [:or [:= :all] [:vector {:min 1} NonBlank]]]])
   (translator :magit/stage-verify stage-verify-code [:map [:directory NonBlank] [:paths [:vector {:min 1} NonBlank]]])
   (translator :magit/commit commit-code [:map [:directory NonBlank] [:message NonBlank] [:all :boolean]])
   (translator :magit/push push-code [:map [:directory NonBlank] [:set-upstream :boolean] [:remote {:optional true} [:maybe NonBlank]]])
   (translator :magit/pull pull-code Base)
   (translator :magit/fetch fetch-code [:map [:directory NonBlank] [:remote {:optional true} [:maybe NonBlank]]])
   (translator :magit/feature-branches feature-branches-code Base)])
