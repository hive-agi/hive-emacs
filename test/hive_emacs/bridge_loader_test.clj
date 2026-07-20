(ns hive-emacs.bridge-loader-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-emacs.bridge-loader :as loader]))

(deftest load-path-form-quotes-directories
  (let [directory "/tmp/hive \"emacs\""
        form (loader/load-path-elisp [directory])]
    (is (str/includes? form (pr-str directory)))
    (is (str/includes? form "add-to-list"))))

(deftest entrypoint-form-requires-and-validates-bridge
  (let [form (loader/load-entrypoints-elisp)]
    (is (str/includes? form "(require 'hive-mcp)"))
    (is (str/includes? form "(require 'hive-mcp-cider)"))
    (is (str/includes? form "hive-mcp-cider-spawn-session"))
    (is (str/includes? form "hive-mcp-cider-eval-in-session"))
    (is (str/includes? form "(hive-mcp-mode 1)"))
    (is (str/includes? form "bound-and-true-p"))))

(deftest ready-bridge-is-not-reloaded
  (let [calls (atom [])
        eval-fn (fn [code timeout]
                  (swap! calls conj [code timeout])
                  {:success true})]
    (is (true? (loader/ensure-loaded! eval-fn)))
    (is (= [[(loader/bridge-ready-elisp) 5000]] @calls))))

(deftest absent-bridge-loads-entrypoints-in-order
  (let [calls (atom [])
        eval-fn (fn [code timeout]
                  (swap! calls conj [code timeout])
                  {:success (not= code (loader/bridge-ready-elisp))})]
    (with-redefs [loader/resolve-elisp-dirs (constantly ["/tmp/elisp"])]
      (is (true? (loader/ensure-loaded! eval-fn))))
    (is (= [5000 5000 15000] (mapv second @calls)))
    (is (= (loader/load-entrypoints-elisp) (ffirst (drop 2 @calls))))))

(deftest failed-load-remains-retryable
  (let [ready? (atom false)
        entry-attempts (atom 0)
        eval-fn (fn [code _timeout]
                  (cond
                    (= code (loader/bridge-ready-elisp))
                    {:success @ready?}

                    (= code (loader/load-entrypoints-elisp))
                    (let [succeeded? (= 2 (swap! entry-attempts inc))]
                      (when succeeded? (reset! ready? true))
                      {:success succeeded?})

                    :else
                    {:success true}))]
    (with-redefs [loader/resolve-elisp-dirs (constantly ["/tmp/elisp"])]
      (is (false? (loader/ensure-loaded! eval-fn)))
      (is (true? (loader/ensure-loaded! eval-fn)))
      (is (true? (loader/ensure-loaded! eval-fn))))
    (is (= 2 @entry-attempts))))

(deftest concurrent-initialization-loads-once
  (let [ready? (atom false)
        entry-attempts (atom 0)
        eval-fn (fn [code _timeout]
                  (cond
                    (= code (loader/bridge-ready-elisp))
                    {:success @ready?}

                    (= code (loader/load-entrypoints-elisp))
                    (do
                      (swap! entry-attempts inc)
                      (Thread/sleep 50)
                      (reset! ready? true)
                      {:success true})

                    :else
                    {:success true}))]
    (with-redefs [loader/resolve-elisp-dirs (constantly ["/tmp/elisp"])]
      (let [a (future (loader/ensure-loaded! eval-fn))
            b (future (loader/ensure-loaded! eval-fn))]
        (is (true? @a))
        (is (true? @b))))
    (is (= 1 @entry-attempts))))