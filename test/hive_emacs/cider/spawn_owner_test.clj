(ns hive-emacs.cider.spawn-owner-test
  "A spawn's outcome is told to the caller that asked for it, never to another
   coordinator window (card 20261008232949-5b0f7835)."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-schemas.test :as hst]
            [hive-emacs.cider.spawn :as spawn]))

(defn- temp-root []
  (doto (java.io.File/createTempFile "spawn-owner" "") (.delete) (.mkdirs)))

(use-fixtures :each
  (fn [t]
    (spawn/reset-watches!)
    (binding [spawn/*root-fn* temp-root]
      (t))
    (spawn/reset-watches!)))

(def ^:private Caller [:enum "coordinator:1" "coordinator:2" "coordinator"])

(def ^:private VisibleInput
  [:map [:owner [:maybe Caller]] [:caller Caller]])

(defn- visible [{:keys [owner caller]}]
  (spawn/visible-to? (cond-> {:name "s"} owner (assoc :owner owner)) caller))

(defn- visible-law [{:keys [owner caller]} out]
  (= out (or (nil? owner) (= owner caller))))

(hst/deftrifecta-from-schema visible-to-contract
  #'visible
  {:in VisibleInput
   :out :boolean
   :rel visible-law
   :num-tests 40
   :seed 0
   :n-cases 6})

(defn- watch [nm]
  {:name nm :port 7000 :repl-type "clj" :project-dir nil
   :requested-ms (System/currentTimeMillis) :deadline-ms 90000})

(deftest a-spawn-outcome-reaches-only-the-caller-that-asked
  (spawn/watch! (spawn/owned (watch "mine") "coordinator:1"))
  (spawn/watch! (spawn/owned (watch "theirs") "coordinator:2"))
  (let [body (spawn/emitter {:caller-id "coordinator:1"})]
    (is (str/includes? body "\"mine\""))
    (is (not (str/includes? body "\"theirs\"")))))

(deftest another-callers-read-consumes-nothing-of-mine
  (spawn/watch! (spawn/owned (assoc (watch "mine") :requested-ms 0 :deadline-ms 1) "coordinator:1"))
  (testing "a foreign caller sees nothing and settles nothing"
    (is (nil? (spawn/emitter {:caller-id "coordinator:2"})))
    (is (contains? (spawn/watches) "mine")))
  (testing "the owner still gets its settled outcome"
    (is (str/includes? (str (spawn/emitter {:caller-id "coordinator:1"})) "\"mine\""))))

(deftest an-unowned-spawn-is-told-to-every-caller-as-before
  (spawn/watch! (watch "legacy"))
  (is (str/includes? (str (spawn/emitter {:caller-id "coordinator:2"})) "\"legacy\"")))

(deftest a-blank-owner-leaves-the-watch-unowned
  (is (not (contains? (spawn/owned (watch "x") "") :owner)))
  (is (= "coordinator:1" (:owner (spawn/owned (watch "x") "coordinator:1")))))
