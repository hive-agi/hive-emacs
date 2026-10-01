(ns hive-emacs.cider.introspection
  "Pure decoding of the bounded nREPL boundary's answers into the shapes the
   `code cider` introspection verbs have always returned."
  (:require [clojure.data.json :as json]
            [hive-dsl.result :as result]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def Envelope
  "The boundary's JSON envelope, parsed: exactly one outcome key."
  [:or
   [:map [:ok [:map-of :string :any]]]
   [:map [:timeout int?]]
   [:map [:error :string]]
   [:map [:refused :string]]])

(m/=> parse-envelope [:=> [:cat :any] [:maybe Envelope]])

(defn parse-envelope
  "RAW (the evaluator's result: a JSON string, possibly printed once more as a
   string literal) as an Envelope, or nil when it is not one. The ok payload
   keeps nREPL's string keys."
  [raw]
  (when (string? raw)
    (let [v (result/rescue nil (json/read-str raw))
          v (if (string? v) (result/rescue nil (json/read-str v)) v)]
      (when (map? v)
        (cond
          (map? (get v "ok")) {:ok (get v "ok")}
          (int? (get v "timeout")) {:timeout (get v "timeout")}
          (string? (get v "error")) {:error (get v "error")}
          (string? (get v "refused")) {:refused (get v "refused")})))))

(def ^:private info-keys
  ["name" "ns" "doc" "arglists" "file" "line" "column" "resource" "macro"
   "special-form" "protocol" "spec" "see-also" "added" "deprecated"])

(defn- arglists [m]
  (or (get m "arglists-str") (get m "arglists")))

(defn- found? [m]
  (some? (get m "name")))

(m/=> decode-doc [:=> [:cat :string [:map-of :string :any]] [:map-of :string :any]])

(defn decode-doc
  "The doc verb's map for SYMBOL from an nREPL info response M."
  [symbol m]
  {"doc" (or (get m "doc") "No documentation available")
   "arglists" (or (arglists m) "")
   "ns" (or (get m "ns") "")
   "name" (or (get m "name") symbol)
   "file" (or (get m "file") "")
   "line" (or (get m "line") 0)})

(m/=> decode-info [:=> [:cat :string [:map-of :string :any]] [:map-of :string :any]])

(defn decode-info
  "The info verb's map for SYMBOL from an nREPL info response M: the known
   keys present, or an error map when nREPL found nothing."
  [symbol m]
  (if (found? m)
    (into {} (keep (fn [k] (when-let [v (if (= "arglists" k) (arglists m) (get m k))]
                             [k v])))
          info-keys)
    {"error" (format "No info found for '%s'" symbol)}))

(m/=> decode-complete [:=> [:cat [:map-of :string :any]] [:vector [:map-of :string :any]]])

(defn- rows
  "The map rows under K in M; anything else there yields no rows."
  [m k]
  (let [v (get m k)]
    (if (sequential? v) (filterv map? v) [])))

(defn decode-complete
  "Completion candidates from an nREPL completions response M."
  [m]
  (mapv #(select-keys % ["candidate" "type" "ns"]) (rows m "completions")))

(m/=> decode-apropos [:=> [:cat [:map-of :string :any]] [:vector [:map-of :string :any]]])

(defn decode-apropos
  "Apropos matches from an nREPL apropos response M."
  [m]
  (mapv (fn [match] {"name" (get match "name")
                     "type" (get match "type")
                     "doc" (or (get match "doc") "")})
        (rows m "apropos-matches")))

(def decoders
  "verb -> (fn [params ok-payload] value)."
  {:doc      (fn [{:keys [symbol]} m] (decode-doc (str symbol) m))
   :info     (fn [{:keys [symbol]} m] (decode-info (str symbol) m))
   :complete (fn [_ m] (decode-complete m))
   :apropos  (fn [_ m] (decode-apropos m))})

(m/=> outcome [:=> [:cat :keyword :map :any] :any])

(defn outcome
  "Classify the boundary's RAW answer to VERB.

   Returns a Result: ok wraps the verb's JSON text; err carries
   :cider/refused (with :repl-type, the caller serves it another way),
   :cider/timeout, or :cider/introspection-failed, each with a :message."
  [verb params raw]
  (let [env (parse-envelope raw)]
    (cond
      (:ok env)
      (result/ok (json/write-str ((get decoders verb) params (:ok env))))

      (:refused env)
      (result/err :cider/refused {:repl-type (:refused env)
                                  :message (str "refused for a " (:refused env) " REPL")})

      (:timeout env)
      (result/err :cider/timeout {:message (str "nREPL " (name verb) " got no reply within "
                                                (:timeout env) "s")})

      (:error env)
      (result/err :cider/introspection-failed {:message (:error env)})

      :else
      (result/err :cider/introspection-failed
                  {:message (str "unexpected answer from the nREPL boundary: " (pr-str raw))}))))
