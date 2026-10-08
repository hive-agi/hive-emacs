(ns hive-emacs.notify
  "Desktop notifications via notify-send (freedesktop.org).
   
   Provides OS-level notifications that appear in the system notification area,
   independent of Emacs. Used for hivemind alerts that require human attention."
  (:require [hive-notify.backends.desktop :as desktop]
            [hive-spi.notify :as notify]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT


;; =============================================================================
;; Desktop Notifications via notify-send
;; =============================================================================

(def ^:dynamic *desktop-backend*
  "Factory port for the desktop backend; bind to a stub backend in tests."
  desktop/desktop-backend)

(defn notify!
  "Send a desktop notification via notify-send.
   
   Options:
     :summary  - Notification title (required)
     :body     - Notification body text (optional)
     :type     - Type: \"info\", \"warning\", \"error\" (default: \"info\")
     :timeout  - Timeout in ms (default: 5000)
     :app-name - Application name (default: \"hive-mcp\")
   
   Returns true on success, false on failure.

   Compatibility note: DesktopBackend currently owns delivery timing and does
   not expose the legacy :timeout option; it is accepted but not forwarded."
  [{:keys [summary body type app-name]
    :or {type "info" app-name "hive-mcp"}}]
  (try
    (let [backend (*desktop-backend* {:app app-name})
          level (get {"info" :info "warning" :warn "error" :error}
                     type :info)
          delivered? (boolean (:delivered? (notify/notify! backend
                                                          {:summary summary
                                                           :body body
                                                           :level level})))]
      (if delivered?
        (log/debug "Notification sent" {:summary summary :type type})
        (log/warn "Desktop notification failed" {:summary summary :type type}))
      delivered?)
    (catch Exception e
      (log/warn "Failed to send notification:" (.getMessage e))
      false)))
