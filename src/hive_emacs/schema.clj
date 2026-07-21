(ns hive-emacs.schema
  "Malli contracts shared by ports, domain logic, tools, and proof facets."
  (:require [hive-schemas.schema :as schemas]
            [malli.error :as me]))

(def HealthScore
  [:int {:min 0 :max 100}])

(def HealthLevel
  [:enum :healthy :degraded :unhealthy])

(def Ling
  [:map {:closed false}
   [:ling/id :string]
   [:ling/project-id {:optional true} [:maybe :string]]
   [:ling/status {:optional true} :keyword]])

(def Task
  [:map {:closed false}
   [:task/id :string]
   [:task/status {:optional true} :keyword]])

(def Tasks
  [:vector Task])

(def PingResult
  [:and
   [:map {:closed false}
    [:success {:optional true} :boolean]
    [:success? {:optional true} :boolean]
    [:duration-ms {:optional true} nat-int?]
    [:error {:optional true} :any]]
   [:fn {:error/message "ping result needs :success or :success?"}
    #(or (contains? % :success) (contains? % :success?))]])

(def RuntimePorts
  [:map {:closed true}
   [:ping-fn [:maybe ifn?]]
   [:emit-fn [:maybe ifn?]]
   [:lookup-ling-fn [:maybe ifn?]]
   [:tasks-for-ling-fn [:maybe ifn?]]
   [:fail-task-fn [:maybe ifn?]]
   [:release-claims-fn [:maybe ifn?]]
   [:update-ling-fn [:maybe ifn?]]
   [:report-daemon-error-fn [:maybe ifn?]]
   [:terminal-dispatch-fn [:maybe ifn?]]
   [:resolve-agent-context-fn [:maybe ifn?]]
   [:capability-fn [:maybe ifn?]]])

(def DaemonStatus
  [:enum :active :stale :error :terminated])

(def Daemon
  [:map {:closed false}
   [:emacs-daemon/id :string]
   [:emacs-daemon/status DaemonStatus]
   [:emacs-daemon/socket-name {:optional true} :string]
   [:emacs-daemon/lings {:optional true} [:set :string]]
   [:emacs-daemon/health-score {:optional true} HealthScore]
   [:emacs-daemon/error-count {:optional true} nat-int?]
   [:emacs-daemon/heartbeat-at {:optional true} inst?]])

(def ScoreInput
  [:map {:closed true}
   [:previous [:maybe HealthScore]]
   [:latency-ms [:maybe nat-int?]]
   [:errors nat-int?]
   [:lings nat-int?]])

(def ScoreSnapshot
  [:map {:closed true}
   [:health-score HealthScore]
   [:health-level HealthLevel]])

(def HeartbeatReport
  [:map {:closed true}
   [:healthy? :boolean]
   [:health-score HealthScore]
   [:health-level HealthLevel]
   [:latency-ms [:maybe nat-int?]]
   [:consecutive-errors nat-int?]
   [:ling-count nat-int?]
   [:success? :boolean]])

(def McpResponse
  [:map {:closed true}
   [:type [:= "text"]]
   [:text :string]
   [:isError {:optional true} :boolean]])

(def Result
  [:multi {:dispatch (fn [value]
                       (cond
                         (and (map? value)
                              (contains? value :ok)
                              (not (contains? value :error))) :ok
                         (and (map? value)
                              (contains? value :error)
                              (not (contains? value :ok))) :error
                         :else :invalid))}
   [:ok [:map {:closed false} [:ok :any]]]
   [:error [:map {:closed false} [:error :qualified-keyword]]]])

(def AddonInitResult
  [:map {:closed false}
   [:success? :boolean]
   [:errors {:optional true} [:vector :string]]
   [:metadata {:optional true} :map]
   [:already-initialized? {:optional true} :boolean]])

(def schema-bundle
  {:hive-emacs/health-score HealthScore
   :hive-emacs/health-level HealthLevel
   :hive-emacs/ling Ling
   :hive-emacs/task Task
   :hive-emacs/tasks Tasks
   :hive-emacs/ping-result PingResult
   :hive-emacs/runtime-ports RuntimePorts
   :hive-emacs/daemon Daemon
   :hive-emacs/score-input ScoreInput
   :hive-emacs/score-snapshot ScoreSnapshot
   :hive-emacs/heartbeat-report HeartbeatReport
   :hive-emacs/mcp-response McpResponse
   :hive-emacs/result Result
   :hive-emacs/addon-init-result AddonInitResult})

(schemas/register-all! schema-bundle)

(defn valid?
  [schema value]
  (schemas/validate schema value))

(defn validate!
  "Return VALUE when it conforms; throw explainable boundary error otherwise."
  [schema value]
  (if (valid? schema value)
    value
    (throw
     (ex-info "hive-emacs contract violation"
              {:schema schema
               :value value
               :explain (some-> (schemas/explain schema value)
                                me/humanize)}))))
