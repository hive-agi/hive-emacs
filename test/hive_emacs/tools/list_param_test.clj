(ns hive-emacs.tools.list-param-test
  "A spawn list param sent as JSON TEXT must decode, never become one entry.

   DIP-in-tests: the spawn path's only host effect is the elisp-eval
   boundary, injected via cider/*eval-fn* with a recording stub."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [hive-emacs.tools.cider :as cider]
            [hive-emacs.tools.list-param :as list-param]
            [hive-test.trifecta :refer [deftrifecta]]
            [clojure.data.json :as json]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(deftrifecta json-array-text
  list-param/json-array-text
  {:golden-path "test/golden/hive_emacs/tools/list_param_json_array_text.edn"
   :cases {:one-alias          "[\"dev\"]"
           :two-entries        "[\"-Srepro\", \"-J-Xmx2g\"]"
           :padded             "  [\"test\"]  "
           :empty-array        "[]"
           :edn-map-text       "{:deps {}}"
           :bare-alias         "dev"
           :colon-aliases      ":dev:test"
           :malformed          "[\"dev\""
           :json-object        "{\"a\": 1}"
           :not-a-string       42
           :nil                nil}
   :gen (gen/one-of [gen/string-alphanumeric
                     (gen/fmap #(str "[" (str/join "," (map pr-str %)) "]")
                               (gen/vector gen/string-alphanumeric))])
   :pred (fn [out] (or (nil? out) (sequential? out)))
   :num-tests 200
   :mutations [["any-string-is-one-entry" (fn [s] (when (string? s) [s]))]
               ["always-nil" (fn [_] nil)]
               ["no-trim" (fn [s]
                            (when (and (string? s) (str/starts-with? s "["))
                              (try (json/read-str s)
                                   (catch Exception _malformed-reads-as-nil nil))))]]})

(deftest a-json-array-round-trips-its-entries
  (doseq [entries [[] ["dev"] ["dev" "test"] ["-J-Xmx2g" "-Srepro"]]]
    (is (= entries (list-param/json-array-text
                    (str "[" (str/join ", " (map pr-str entries)) "]"))))))

(defn- recording-eval
  "A stub *eval-fn* answering every elisp form with an empty JSON object and
   recording each form in CALLS."
  [calls]
  (fn
    ([code] (swap! calls conj code) {:success true :result "\"{}\""})
    ([code _timeout-ms] (swap! calls conj code) {:success true :result "\"{}\""})))

(deftest spawn-decodes-list-params-sent-as-json-text
  ;; Witness for the measured failure: aliases ["dev"] arrived as the TEXT
  ;; "[\"dev\"]", became the alias name `["dev"]`, and the spawn ran
  ;; -M:["dev"] with an empty-keyword neutralizer (Invalid token: :).
  (let [calls (atom [])]
    (binding [cider/*eval-fn* (recording-eval calls)]
      (cider/handle-spawn {:name "s"
                           :aliases "[\"dev\"]"
                           :extra_args "[\"-Srepro\", \"-J-Xmx2g\"]"
                           :extra_deps "[\"{:deps {my/lib {:local/root \\\"../lib\\\"}}}\"]"
                           :middleware " [\"refactor-nrepl.middleware/wrap-refactor\"] "})
      (let [form (first @calls)]
        (is (str/includes? form ":aliases '(\"dev\")") form)
        (is (str/includes? form ":extra-args '(\"-Srepro\" \"-J-Xmx2g\")") form)
        (is (str/includes? form ":extra-deps '(\"{:deps {my/lib") form)
        (is (str/includes? form ":middleware '(\"refactor-nrepl.middleware/wrap-refactor\")") form)
        (is (not (str/includes? form "\"[")) "no entry is the raw array text")))))

(deftest a-plain-string-keeps-its-old-meaning
  (let [calls (atom [])]
    (binding [cider/*eval-fn* (recording-eval calls)]
      (cider/handle-spawn {:name "s" :aliases ":dev:test" :extra_deps "{:deps {}}"})
      (let [form (first @calls)]
        (is (str/includes? form ":aliases '(\"dev\" \"test\")") form)
        (is (str/includes? form ":extra-deps '(\"{:deps {}}\")") form)))))
