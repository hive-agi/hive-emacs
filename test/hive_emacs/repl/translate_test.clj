(ns hive-emacs.repl.translate-test
  "Contract, generative and DIP/OCP checks for the nREPL<->Slynk layer."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-dsl.result :as result]
            [hive-emacs.repl.boundary :as boundary]
            [hive-emacs.repl.profile :as profile]
            [hive-emacs.repl.schema :as schema]
            [hive-emacs.repl.translate :as translate]
            [hive-schemas.test :as schema-test]
            [malli.core :as m]))

(use-fixtures :each (fn [f] (profile/reset-registry!) (f) (profile/reset-registry!)))

;;; =============================================================================
;;; Schema-derived coverage
;;; =============================================================================

(defn- renders-callee?
  [call out]
  (and (str/starts-with? out "(")
       (str/ends-with? out ")")
       (str/includes? out (:call/rpc call))))

(schema-test/deftrifecta-from-schema call-form-proof
  boundary/call-form
  {:in :hive-emacs.repl/call
   :out :string
   :rel renders-callee?
   :contract true
   :mutation false
   :num-tests 100
   :seed 42})

(schema-test/deftrifecta-predicate valid-profile-proof
  schema/valid-profile?
  {:schema :hive-emacs.repl/profile})

;;; =============================================================================
;;; The translation table — what a schema cannot state
;;; =============================================================================

(deftest every-registered-profile-conforms
  (doseq [p profile/default-profiles]
    (testing (:profile/label p)
      (is (m/validate schema/Profile p)))))

(deftest slynk-package-qualification-is-preserved
  (testing "the two contrib packages are not SLYNK, and must survive planning"
    (doseq [[verb expected params]
            [[:complete "slynk-completion:simple-completions" {:prefix "map"}]
             [:apropos  "slynk-apropos:apropos-list-for-emacs" {:pattern "x"}]
             [:eval     "slynk:eval-and-grab-output" {:code "1"}]]]
      (let [r (translate/plan {:req/verb verb :req/backend :slynk :req/params params})]
        (is (result/ok? r))
        (is (= expected (get-in (:ok r) [:plan/call :call/rpc])))))))

(deftest prelude-is-carried-into-the-plan
  (testing "contrib modules must load before their ops resolve"
    (let [r (translate/plan {:req/verb :complete :req/backend :slynk
                             :req/params {:prefix "map"}})]
      (is (= ["slynk/completion" "slynk/apropos"] (:plan/prelude (:ok r)))))))

(deftest defaults-may-legitimately-be-nil
  (testing "apropos defaults :package to nil, which counts as supplied"
    (let [r (translate/plan {:req/verb :apropos :req/backend :slynk
                             :req/params {:pattern "mapcar"}})]
      (is (result/ok? r))
      (is (= ["mapcar" true false nil] (get-in (:ok r) [:plan/call :call/args]))))))

;;; =============================================================================
;;; The lang dimension
;;; =============================================================================

(deftest clojure-source-is-wrapped-cl-source-is-not
  (testing ":cl passes through untouched"
    (let [r (translate/plan {:req/verb :eval :req/backend :slynk :req/lang :cl
                             :req/params {:code "(+ 1 2)"}})]
      (is (= ["(+ 1 2)"] (get-in (:ok r) [:plan/call :call/args])))))
  (testing ":clojure is wrapped in the reader/compiler form"
    (let [r (translate/plan {:req/verb :eval :req/backend :slynk :req/lang :clojure
                             :req/params {:code "(map inc [1 2])"}})
          arg (first (get-in (:ok r) [:plan/call :call/args]))]
      (is (str/includes? arg "cloture::compile-and-eval"))
      (is (str/includes? arg "named-readtables:find-readtable"))
      (is (str/includes? arg "(read-from-string \"(map inc [1 2])\")")
          "source is embedded as a CL string literal")))
  (testing "only :eval is wrapped"
    (let [r (translate/plan {:req/verb :doc :req/backend :slynk :req/lang :clojure
                             :req/params {:symbol "car"}})]
      (is (= ["car"] (get-in (:ok r) [:plan/call :call/args]))))))

(deftest cl-string-escapes-quotes-and-backslashes
  (is (= "\"a\\\"b\"" (translate/cl-string "a\"b")))
  (is (= "\"a\\\\b\"" (translate/cl-string "a\\b"))))

;;; =============================================================================
;;; Failure modes
;;; =============================================================================

(deftest failures-are-typed
  (testing "a backend nobody registered"
    (let [r (translate/plan {:req/verb :eval :req/backend :nope :req/params {:code "1"}})]
      (is (not (result/ok? r)))
      (is (= :unknown-backend (:fail/kind r)))))
  (testing "a verb this backend does not serve"
    (let [r (translate/plan {:req/verb :inspect :req/backend :cider :req/params {:form "x"}})]
      (is (= :unsupported-verb (:fail/kind r)))))
  (testing "a required argument the caller omitted"
    (let [r (translate/plan {:req/verb :doc :req/backend :slynk})]
      (is (= :missing-param (:fail/kind r))))))

;;; =============================================================================
;;; DIP / OCP — a third backend is data, not a code change
;;; =============================================================================

(deftest registering-a-third-backend-changes-no-code
  (let [geiser {:profile/id :geiser
                :profile/label "Geiser (Scheme)"
                :profile/default-timeout-ms 15000
                :profile/ops {:eval {:op/rpc "geiser:eval"
                                     :op/args [:code]
                                     :op/shape :string}}}]
    (is (= :geiser (profile/register! geiser)))
    (testing "it plans through the same untouched translator"
      (let [r (translate/plan {:req/verb :eval :req/backend :geiser
                               :req/params {:code "(+ 1 2)"}})]
        (is (result/ok? r))
        (is (= "geiser:eval" (get-in (:ok r) [:plan/call :call/rpc])))
        (is (= 15000 (get-in (:ok r) [:plan/call :call/timeout-ms])))))
    (testing "and reports only the capabilities it declared"
      (is (= [:eval] (profile/capabilities :geiser)))
      (is (not (profile/supports? :geiser :apropos))))))

(deftest an-invalid-profile-is-refused
  (is (thrown? clojure.lang.ExceptionInfo
               (profile/register! {:profile/id :broken}))))

;;; =============================================================================
;;; Boundary — injected stub, never a live host
;;; =============================================================================

(deftest boundary-loads-prelude-before-the-call
  (let [calls (atom [])
        stub (fn [elisp timeout-ms]
               (swap! calls conj {:elisp elisp :timeout timeout-ms})
               {:success true :result '(:ok "stubbed")})]
    (binding [boundary/*eval-fn* stub]
      (let [r (boundary/run {:req/verb :complete :req/backend :slynk
                             :req/params {:prefix "map"}})]
        (is (result/ok? r))
        (is (= :completion-list (:shape (:ok r))))
        (is (= 3 (count @calls)) "two prelude loads, then the op")
        (is (str/includes? (:elisp (nth @calls 0)) "slynk/completion"))
        (is (str/includes? (:elisp (nth @calls 1)) "slynk/apropos"))
        (is (str/includes? (:elisp (nth @calls 2)) "simple-completions"))))))

(deftest every-emitted-request-is-deadline-bounded
  (testing "an unresolvable callee is never answered, so the wait must be bounded"
    (let [elisp (boundary/bounded-elisp "(slynk:whatever)" 20000)]
      (is (str/includes? elisp "with-timeout"))
      (is (str/includes? elisp "sly-eval-async"))
      (is (not (str/includes? elisp "(sly-eval "))
          "bare sly-eval blocks the editor with no deadline"))))

(deftest transport-failure-surfaces-as-a-typed-error
  (binding [boundary/*eval-fn* (fn [_ _] {:success false :error "no connection"})]
    (let [r (boundary/run {:req/verb :status :req/backend :slynk})]
      (is (not (result/ok? r)))
      (is (= :transport (:fail/kind r))))))

;;; =============================================================================
;;; Lisp literal encoding
;;; =============================================================================

(deftest clojure-booleans-become-cl-literals
  (is (= "cl:t" (boundary/lisp-arg true)))
  (is (= "cl:nil" (boundary/lisp-arg false)))
  (is (= "cl:nil" (boundary/lisp-arg nil)))
  (is (= "\"x\"" (boundary/lisp-arg "x")))
  (is (= ":kw" (boundary/lisp-arg :kw)))
  (is (= "42" (boundary/lisp-arg 42))))
