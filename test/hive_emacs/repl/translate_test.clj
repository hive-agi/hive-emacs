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
  (testing "an op's contrib module must load before it resolves"
    (let [r (translate/plan {:req/verb :complete :req/backend :slynk
                             :req/params {:prefix "map"}})]
      (is (= ["slynk/completion"] (:plan/prelude (:ok r))))))
  (testing "a module is attached only to the op that needs it"
    (doseq [[verb params expected]
            [[:apropos {:pattern "x"} ["slynk/apropos"]]
             [:eval {:code "1"} []]
             [:status {} []]
             [:doc {:symbol "car"} []]]]
      (let [r (translate/plan {:req/verb verb :req/backend :slynk :req/params params})]
        (is (= expected (:plan/prelude (:ok r))) (str verb))))))

(deftest profile-prelude-and-op-requires-compose
  (profile/register! {:profile/id :composed
                      :profile/label "composed"
                      :profile/default-timeout-ms 1000
                      :profile/prelude ["base" "shared"]
                      :profile/ops {:eval {:op/rpc "x:eval" :op/args [:code]
                                           :op/requires ["shared" "own"]
                                           :op/shape :string}
                                    :status {:op/rpc "x:status" :op/args []
                                             :op/shape :string}}})
  (is (= ["base" "shared" "own"]
         (:plan/prelude (:ok (translate/plan {:req/verb :eval :req/backend :composed
                                              :req/params {:code "1"}})))))
  (is (= ["base" "shared"]
         (:plan/prelude (:ok (translate/plan {:req/verb :status :req/backend :composed}))))))

(deftest every-default-op-plans-a-valid-plan
  (doseq [p profile/default-profiles
          [verb op] (:profile/ops p)]
    (let [params (zipmap (:op/args op) (repeat "x"))
          r (translate/plan {:req/verb verb :req/backend (:profile/id p) :req/params params})]
      (is (result/ok? r) (str (:profile/id p) " " verb))
      (is (schema/valid-plan? (:ok r)) (str (:profile/id p) " " verb))
      (is (= (vec (distinct (concat (:profile/prelude p []) (:op/requires op []))))
             (:plan/prelude (:ok r)))
          (str (:profile/id p) " " verb)))))

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

(defn- recording-stub
  [calls reply-fn]
  (fn [elisp timeout-ms]
    (swap! calls conj {:elisp elisp :timeout timeout-ms})
    (reply-fn elisp)))

(deftest boundary-loads-prelude-before-the-call
  (let [calls (atom [])]
    (binding [boundary/*eval-fn* (recording-stub calls (constantly {:success true :result '(:ok "stubbed")}))]
      (let [r (boundary/run {:req/verb :complete :req/backend :slynk
                             :req/params {:prefix "map"}})]
        (is (result/ok? r))
        (is (= :completion-list (:shape (:ok r))))
        (is (= 2 (count @calls)) "the op's one module, then the op")
        (is (str/includes? (:elisp (nth @calls 0)) "slynk/completion"))
        (is (str/includes? (:elisp (nth @calls 1)) "simple-completions"))))))

(deftest an-unloadable-module-fails-only-the-op-that-needs-it
  (let [calls (atom [])
        apropos-dead (fn [elisp]
                       (if (str/includes? elisp "slynk/apropos")
                         {:success false :error "timeout loading slynk/apropos"}
                         {:success true :result '(:ok "fine")}))]
    (binding [boundary/*eval-fn* (recording-stub calls apropos-dead)]
      (is (= :transport (:fail/kind (boundary/run {:req/verb :apropos :req/backend :slynk
                                                   :req/params {:pattern "x"}}))))
      (doseq [[verb params] [[:eval {:code "1"}] [:status {}] [:doc {:symbol "car"}]
                             [:complete {:prefix "ma"}]]]
        (is (result/ok? (boundary/run {:req/verb verb :req/backend :slynk :req/params params}))
            (str verb " survives the dead apropos module")))
      (is (not-any? #(str/includes? (:elisp %) "slynk/apropos")
                    (rest @calls))
          "no other verb tried to load the dead module"))))

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

;;; =============================================================================
;;; The :cider profile executes through the same bounded boundary
;;; =============================================================================

(deftest cider-plans-carry-the-nrepl-transport
  (doseq [[verb params op wire]
          [[:eval {:code "(+ 1 2)"} "eval" "code"]
           [:info {:symbol "map"} "info" "sym"]
           [:complete {:prefix "ma"} "completions" "prefix"]
           [:apropos {:pattern "ma"} "apropos" "query"]]]
    (let [call (:plan/call (:ok (translate/plan {:req/verb verb :req/backend :cider
                                                 :req/params params})))
          form (boundary/nrepl-request-form call)]
      (is (= :nrepl (:call/transport call)) (str verb))
      (is (str/starts-with? form (str "(list \"op\" \"" op "\" \"" wire "\" ")) form))))

(deftest every-cider-request-is-deadline-bounded
  (let [calls (atom [])]
    (binding [boundary/*eval-fn* (recording-stub calls (constantly {:success true :result '(:ok nil)}))]
      (doseq [verb (profile/capabilities :cider)]
        (let [op (get-in profile/cider-profile [:profile/ops verb])]
          (is (result/ok? (boundary/run {:req/verb verb :req/backend :cider
                                         :req/params (zipmap (:op/args op) (repeat "x"))}))
              (str verb)))))
    (is (= (count (profile/capabilities :cider)) (count @calls))
        "no prelude on nREPL: one request per verb")
    (doseq [{:keys [elisp timeout]} @calls]
      (is (str/includes? elisp "cider-nrepl-send-request"))
      (is (str/includes? elisp "with-timeout"))
      (is (not (str/includes? elisp "cider-nrepl-sync-request"))
          "a sync request waits with no deadline")
      (is (= 60000 timeout)))))

(deftest cider-request-deadline-follows-the-request
  (let [calls (atom [])]
    (binding [boundary/*eval-fn* (recording-stub calls (constantly {:success true :result '(:ok nil)}))]
      (boundary/run {:req/verb :eval :req/backend :cider :req/params {:code "1"}
                     :req/timeout-ms 2500}))
    (is (str/includes? (:elisp (first @calls)) "(with-timeout (3 "))
    (is (= 2500 (:timeout (first @calls))))))

(deftest nrepl-request-encoding
  (testing "strings are escaped as elisp literals, integers stay integers"
    (is (= "\"a\\\"b\"" (boundary/nrepl-value "a\"b")))
    (is (= "\"line\\n\"" (boundary/nrepl-value "line\n")))
    (is (= "42" (boundary/nrepl-value 42)))
    (is (= "\"kw\"" (boundary/nrepl-value :kw))))
  (testing "an argument with no wire key is refused, not silently dropped"
    (is (thrown? clojure.lang.ExceptionInfo
                 (boundary/nrepl-request-form {:call/rpc "nrepl/eval" :call/args ["1"]
                                               :call/transport :nrepl :call/wire-keys []
                                               :call/shape :plist :call/timeout-ms 1000})))))

(deftest a-registered-transport-is-a-defmethod-not-an-edit
  (let [calls (atom [])
        plan {:plan/prelude []
              :plan/call {:call/rpc "p:x" :call/args []
                          :call/transport ::probe
                          :call/shape :any :call/timeout-ms 1000}}]
    (defmethod boundary/call-elisp ::probe [call] (str "probe:" (:call/rpc call)))
    (try
      (is (schema/valid-plan? plan))
      (binding [boundary/*eval-fn* (recording-stub calls (constantly {:success true :result "ok"}))]
        (is (result/ok? (boundary/execute plan))))
      (is (= "probe:p:x" (:elisp (first @calls))))
      (finally (remove-method boundary/call-elisp ::probe)))))
