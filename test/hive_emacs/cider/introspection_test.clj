(ns hive-emacs.cider.introspection-test
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-dsl.result :as result]
            [hive-emacs.cider.introspection :as intro]))

;; Envelopes as the live boundary produced them against a clj nREPL
;; (CIDER 2.1, cider-nrepl), 2026-09-30. Long docstrings trimmed.
(def ^:private live
  {:info    "{\"ok\":{\"added\":\"1.0\",\"arglists-str\":\"[f]\\n[f & colls]\",\"column\":1,\"doc\":\"Returns the result of applying concat\",\"file\":\"jar:file:/clojure/core.clj\",\"line\":2804,\"name\":\"mapcat\",\"ns\":\"clojure.core\",\"resource\":\"clojure/core.clj\",\"see-also\":[\"clojure.core/map\",\"clojure.core/concat\"],\"static\":\"true\",\"status\":[\"done\"]}}"
   :no-info "{\"ok\":{\"status\":[\"done\",\"no-info\"]}}"
   :compl   "{\"ok\":{\"completions\":[{\"candidate\":\"mapcat\",\"ns\":\"clojure.core\",\"priority\":0,\"type\":\"function\"}],\"status\":[\"done\"]}}"
   :apropos "{\"ok\":{\"apropos-matches\":[{\"doc\":\"Returns the result of applying concat.\",\"name\":\"clojure.core/mapcat\",\"type\":\"function\"}],\"status\":[\"done\"]}}"
   :gone    "{\"error\":\"REPL buffer *no-such-buffer* is gone\"}"
   :refused "{\"refused\":\"cljel\"}"
   :timeout "{\"timeout\":2}"})

(defn- ok-json [verb params raw]
  (let [r (intro/outcome verb params raw)]
    (is (result/ok? r) (pr-str r))
    (json/read-str (:ok r))))

(deftest live-envelopes-decode-into-the-verb-shapes
  (testing "doc keeps the bridge's keys and gains the arglists the bridge lost"
    (is (= {"doc" "Returns the result of applying concat" "arglists" "[f]\n[f & colls]"
            "ns" "clojure.core" "name" "mapcat" "file" "jar:file:/clojure/core.clj" "line" 2804}
           (ok-json :doc {:symbol "mapcat"} (:info live)))))
  (testing "info carries the known keys and nothing nREPL-internal"
    (let [m (ok-json :info {:symbol "mapcat"} (:info live))]
      (is (= ["clojure.core/map" "clojure.core/concat"] (get m "see-also")))
      (is (= 2804 (get m "line")))
      (is (not-any? #{"status" "static" "arglists-str"} (keys m)))))
  (testing "an unknown symbol is the bridge's error map"
    (is (= {"error" "No info found for 'nope'"} (ok-json :info {:symbol "nope"} (:no-info live))))
    (is (= "No documentation available" (get (ok-json :doc {:symbol "nope"} (:no-info live)) "doc"))))
  (testing "completions and apropos become vectors of the bridge's rows"
    (is (= [{"candidate" "mapcat" "ns" "clojure.core" "type" "function"}]
           (ok-json :complete {:prefix "mapc"} (:compl live))))
    (is (= [{"name" "clojure.core/mapcat" "type" "function" "doc" "Returns the result of applying concat."}]
           (ok-json :apropos {:pattern "mapcat"} (:apropos live))))))

(deftest non-ok-envelopes-are-typed
  (is (= :cider/refused (:error (intro/outcome :doc {:symbol "x"} (:refused live)))))
  (is (= "cljel" (:repl-type (intro/outcome :doc {:symbol "x"} (:refused live)))))
  (let [r (intro/outcome :complete {:prefix "m"} (:timeout live))]
    (is (= :cider/timeout (:error r)))
    (is (= "nREPL complete got no reply within 2s" (:message r))))
  (let [r (intro/outcome :info {:symbol "x"} (:gone live))]
    (is (= :cider/introspection-failed (:error r)))
    (is (= "REPL buffer *no-such-buffer* is gone" (:message r)))))

(deftest a-string-printed-twice-still-parses
  (is (= {:timeout 2} (intro/parse-envelope (json/write-str (:timeout live))))))

(deftest garbage-is-an-error-not-a-throw
  (doseq [raw [nil "" "nil" "{}" "[1,2]" "{\"ok\":3}" "(:ok nil)" 42]]
    (let [r (intro/outcome :doc {:symbol "x"} raw)]
      (is (= :cider/introspection-failed (:error r)) (pr-str raw)))))

(def ^:private json-scalar
  (gen/one-of [gen/string-alphanumeric gen/small-integer gen/boolean (gen/return nil)]))

(def ^:private json-value
  (gen/resize 8 (gen/recursive-gen
                 (fn [inner] (gen/one-of [(gen/vector inner 0 4)
                                          (gen/map gen/string-alphanumeric inner {:max-elements 4})]))
                 json-scalar)))

(defspec parse-envelope-is-total 200
  (prop/for-all [raw (gen/one-of [gen/string
                                  (gen/fmap json/write-str json-value)
                                  (gen/fmap #(json/write-str {% 1}) (gen/elements ["ok" "timeout" "error" "refused"]))
                                  gen/small-integer])]
    (let [v (intro/parse-envelope raw)]
      (or (nil? v) (= 1 (count v))))))

(defspec decoders-never-throw-on-any-ok-payload 100
  (prop/for-all [payload (gen/map (gen/one-of [(gen/elements ["name" "ns" "doc" "arglists-str" "file" "line"
                                                              "see-also" "completions" "apropos-matches"])
                                               gen/string-alphanumeric])
                                  json-value
                                  {:max-elements 6})
                 verb (gen/elements [:doc :info :complete :apropos])]
    (result/ok? (intro/outcome verb {:symbol "s"} (json/write-str {"ok" payload})))))

(deftest list-decoders-tolerate-rows-of-any-shape
  (is (= [{"candidate" "a"} {}]
         (intro/decode-complete {"completions" [{"candidate" "a" "priority" 0} {"x" 1}]})))
  (is (= [] (intro/decode-complete {})))
  (is (= [] (intro/decode-apropos {"apropos-matches" nil}))))
