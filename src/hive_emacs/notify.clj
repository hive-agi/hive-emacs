(ns hive-emacs.notify
  "Desktop notifications via notify-send (freedesktop.org).
   
   Provides OS-level notifications that appear in the system notification area,
   independent of Emacs. Used for hivemind alerts that require human attention."
  (:require [hive-notify.backends.desktop :as desktop]
            [hive-spi.notify :as notify]
            [taoensso.timbre :as log]
            [hive-emacs.elisp :as el]
            [hive-notify.backends.widget :as widget]
            [hive-emacs.client :as ec]))
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

(def ^:dynamic *eval-fn*
  "Elisp evaluation boundary (DIP): (f code) -> {:success bool :result any
   :error any}. Tests bind a stub recording calls; mirrors
   hive-emacs.tools.emacs/*eval-fn*."
  (fn [code] (ec/eval-elisp code)))

(defn- frame-message
  "Text shown in the Emacs echo area: `[level] summary: body`."
  [{:keys [summary body level]}]
  (str "[" (name (or level :info)) "] " summary
       (when (seq body) (str ": " body))))

(defn frame-notify!
  "Display a notification inside the live Emacs frame (echo area / *Messages*).

   Takes the hive-spi.notify notification shape {:summary :body :level} and is
   the symbol WidgetBackend resolves lazily as its :bridge-sym. Unlike notify!,
   which is DESKTOP delivery through notify-send, this one needs a running
   Emacs. Returns true when Emacs evaluated the message; throws ex-info when it
   did not, because WidgetBackend reports a thrown bridge as not delivered."
  [notification]
  (let [elisp (el/format-elisp "(progn (message \"%%s\" %s) t)"
                               (el/elisp-quote (frame-message notification)))
        resp (*eval-fn* elisp)]
    (when-not (:success resp)
      (throw (ex-info "Emacs frame notification failed"
                      {:reason :emacs-eval-failed :error (:error resp)})))
    true))

(def frame-bridge-sym
  "Fully-qualified symbol of frame-notify!, handed to WidgetBackend."
  'hive-emacs.notify/frame-notify!)

(defn widget-backend
  "WidgetBackend wired to frame-notify!. opts are forwarded to
   hive-notify.backends.widget/widget-backend (e.g. :accept?)."
  ([] (widget-backend {}))
  ([opts]
   (widget/widget-backend (merge {:bridge-sym frame-bridge-sym} opts))))
