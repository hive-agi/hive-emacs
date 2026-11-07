(ns hive-emacs.cider.direct-eval
  "Evaluate code in an nREPL the caller already named by port, over a socket
   from this JVM, under a deadline.

   Why not through Emacs: the bridge eval (`hive-mcp-cider-eval-in-session`)
   is a synchronous poll inside the Emacs command loop, reached through an
   emacsclient call. When Emacs is busy (a large unread nREPL stream, a stuck
   timer) every emacsclient call times out at the client default, so an eval
   failed with \"Emacsclient call timed out after 5000ms\" while the nREPL
   itself answered at once. A named port needs no Emacs at all.

   Effects live behind `*connect-fn*` so tests can drive a real in-process
   nREPL server or a stub."
  (:require [clojure.string :as str]
            [hive-dsl.result :as result]
            [nrepl.core :as nrepl]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def ^:dynamic *connect-fn*
  "Transport boundary: (f host port) -> an nREPL transport (java.io.Closeable).
   Throws when nothing listens there."
  (fn [host port] (nrepl/connect :host host :port port)))

(def default-timeout-ms
  "Deadline for one eval when the caller names none."
  60000)

(defn fold-responses
  "The eval outcome of nREPL RESPONSES: {:value v :out o :err e :ex ex :done? b}.
   value is the last value (the result of the last form); out and err
   accumulate in order."
  [responses]
  (reduce (fn [acc {:keys [value out err ex status]}]
            (cond-> acc
              value (assoc :value value)
              out   (update :out str out)
              err   (update :err str err)
              ex    (assoc :ex ex)
              (some #{"done" :done} status) (assoc :done? true)))
          {:value nil :out "" :err "" :ex nil :done? false}
          responses))

(defn outcome->text
  "The text an MCP eval answers with: printed output, then the value."
  [{:keys [value out]}]
  (str (when-not (str/blank? out) out) (or value "nil")))

(defn- interrupt! [transport session id]
  (try
    (nrepl/message (nrepl/client transport 1000)
                   {:op "interrupt" :session session :interrupt-id id})
    true
    (catch Throwable _ false)))

(defn eval-on-port
  "Result of evaluating CODE in the nREPL on HOST:PORT within TIMEOUT-MS.
   ok text on success; :cider/eval-error with the error text when the code
   threw; :cider/eval-timeout (an nREPL interrupt sent) when no `done`
   arrived in time; :cider/port-unreachable when nothing answered the port.
   NS, when given, is the namespace the code is read in."
  ([port code] (eval-on-port port code nil))
  ([port code {:keys [host timeout-ms ns] :or {host "127.0.0.1"}}]
   (let [timeout-ms (or timeout-ms default-timeout-ms)
         transport  (try (*connect-fn* host port)
                         (catch Throwable t t))]
     (if (instance? Throwable transport)
       (result/err :cider/port-unreachable
                   {:message (str "no nREPL answered on " host ":" port ": "
                                  (.getMessage ^Throwable transport))})
       (with-open [^java.io.Closeable t transport]
         (let [client  (nrepl/client t timeout-ms)
               session (nrepl/new-session client)
               id      (str (java.util.UUID/randomUUID))
               msg     (cond-> {:op "eval" :code code :session session :id id}
                         ns (assoc :ns ns))
               work    (future (fold-responses (nrepl/message client msg)))
               folded  (deref work timeout-ms ::timeout)]
           (cond
             (or (= ::timeout folded) (not (:done? folded)))
             (let [sent? (interrupt! t session id)]
               (future-cancel work)
               (result/err :cider/eval-timeout
                           {:timed-out true
                            :message (str "nREPL eval on port " port " did not finish within "
                                          timeout-ms "ms; interrupt "
                                          (if sent? "sent" "NOT sent"))}))

             (or (:ex folded) (not (str/blank? (:err folded))))
             (result/err :cider/eval-error
                         {:message (str (when-not (str/blank? (:out folded)) (:out folded))
                                        (if (str/blank? (:err folded))
                                          (str "Exception: " (:ex folded))
                                          (:err folded)))})

             :else
             (result/ok (outcome->text folded)))))))))
