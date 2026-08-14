(ns hive-emacs.tools.cider-backend-test
  "The :backend param routes a verb to a transport without disturbing CIDER."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-emacs.tools.cider :as cider]))

(defn- capture
  "Invoke the handler for VERB with PARAMS, recording every elisp string the
   boundary emits. Returns {:elisp [...] :response r}."
  [verb params]
  (let [seen (atom [])
        stub (fn [& args]
               (swap! seen conj (first args))
               {:success true :result "(:ok \"stubbed\")"})]
    (binding [cider/*eval-fn* stub]
      (let [response ((get cider/handlers verb) params)]
        {:elisp @seen :response response}))))

(deftest absent-backend-still-goes-to-cider
  (testing "the default path is untouched by the translation layer"
    (let [{:keys [elisp]} (capture :eval {:code "(+ 1 2)"})]
      (is (seq elisp))
      (is (some #(str/includes? % "hive-mcp-cider") elisp))
      (is (not-any? #(str/includes? % "sly-eval-async") elisp)))))

(deftest explicit-cider-backend-is-the-same-path
  (let [{:keys [elisp]} (capture :eval {:code "(+ 1 2)" :backend "cider"})]
    (is (some #(str/includes? % "hive-mcp-cider") elisp))
    (is (not-any? #(str/includes? % "sly-eval-async") elisp))))

(deftest slynk-backend-routes-through-the-translation-layer
  (let [{:keys [elisp]} (capture :eval {:code "(+ 1 2)" :backend "slynk"})]
    (testing "every emitted request is deadline bounded"
      (is (every? #(str/includes? % "with-timeout") elisp))
      (is (every? #(str/includes? % "sly-eval-async") elisp)))
    (testing "the prelude loads the contrib packages before the op"
      (is (str/includes? (nth elisp 0) "slynk/completion"))
      (is (str/includes? (nth elisp 1) "slynk/apropos")))
    (testing "and the op is the measured callee"
      (is (str/includes? (last elisp) "slynk:eval-and-grab-output")))
    (is (not-any? #(str/includes? % "hive-mcp-cider") elisp))))

(deftest lang-clojure-wraps-source-for-a-cloture-image
  (let [{:keys [elisp]} (capture :eval {:code "(map inc [1 2])"
                                        :backend "slynk" :lang "clojure"})]
    (is (str/includes? (last elisp) "cloture::compile-and-eval"))
    (is (str/includes? (last elisp) "named-readtables:find-readtable")))
  (testing "lang cl is passed through"
    (let [{:keys [elisp]} (capture :eval {:code "(+ 1 2)" :backend "slynk" :lang "cl"})]
      (is (not (str/includes? (last elisp) "cloture::compile-and-eval"))))))

(deftest blank-params-do-not-shadow-an-ops-default
  (testing "an omitted package must fall back to the profile default"
    (let [{:keys [elisp]} (capture :complete {:prefix "map" :backend "slynk" :package ""})]
      (is (str/includes? (last elisp) "COMMON-LISP-USER")))))

(deftest a-verb-the-backend-cannot-serve-is-reported-not-attempted
  (let [{:keys [elisp response]} (capture :inspect {:form "(list 1)" :backend "cider"})]
    (is (empty? elisp) "no request may be issued for an unsupported verb")
    (is (str/includes? (pr-str response) "does not serve"))))

(deftest backend-only-verbs-name-the-backend-that-serves-them
  (testing "the default backend does not serve it, and the error says which does"
    (let [{:keys [elisp response]} (capture :restart {:level 1 :n 0})]
      (is (empty? elisp) "no request may be issued for an unsupported verb")
      (is (str/includes? (pr-str response) "does not serve"))
      (is (str/includes? (pr-str response) "slynk")))))

(deftest session-lifecycle-is-not-part-of-the-verb-vocabulary
  (testing "spawn/connect/sessions stay CIDER-only and ignore :backend"
    (doseq [verb [:spawn :connect :sessions :kill-session :kill-all]]
      (is (contains? cider/handlers verb))))
  (testing "the translatable verbs are the ones a profile can serve"
    (doseq [verb [:eval :doc :info :complete :apropos :status :load-file :inspect :restart]]
      (is (contains? cider/handlers verb)))))
