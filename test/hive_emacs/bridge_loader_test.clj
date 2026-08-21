(ns hive-emacs.bridge-loader-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-emacs.bridge-loader :as loader]))

(defn- temp-jar-with
  "Write a throwaway jar containing ENTRIES (name -> content string)."
  ^java.io.File [entries]
  (let [f (java.io.File/createTempFile "bridge-loader-test" ".jar")]
    (.deleteOnExit f)
    (with-open [out (java.util.jar.JarOutputStream. (io/output-stream f))]
      (doseq [[entry-name content] entries]
        (.putNextEntry out (java.util.jar.JarEntry. ^String entry-name))
        (.write out (.getBytes ^String content))
        (.closeEntry out)))
    f))

(deftest resolve-elisp-dirs-file-urls-resolve-to-containing-dir
  (let [urls {"hive-mcp-addons.el"
              (io/as-url (io/file "/tmp/bridge-elisp/hive-mcp-addons.el"))
              "clojure-elisp/clojure-elisp-runtime.el"
              (io/as-url (io/file "/tmp/rt/clojure-elisp/clojure-elisp-runtime.el"))}]
    (is (= ["/tmp/bridge-elisp" "/tmp/rt/clojure-elisp"]
           (loader/resolve-elisp-dirs urls)))))

(deftest resolve-elisp-dirs-skips-missing-resources
  (is (= [] (loader/resolve-elisp-dirs (constantly nil)))))

(deftest resolve-elisp-dirs-jar-urls-extract-sibling-elisp
  (let [jar (temp-jar-with {"hive-mcp-addons.el" ";; marker"
                            "hive-mcp-cider.el" ";; cider"
                            "cljel/nested.el" ";; nested"
                            "pom.xml" "<project/>"})
        url (java.net.URL. (str "jar:" (.toURI jar) "!/hive-mcp-addons.el"))
        [dir :as dirs] (loader/resolve-elisp-dirs {"hive-mcp-addons.el" url})]
    (is (= 1 (count dirs)))
    (is (.exists (io/file dir "hive-mcp-addons.el")))
    (is (.exists (io/file dir "hive-mcp-cider.el")))
    (is (not (.exists (io/file dir "nested.el"))))
    (is (not (.exists (io/file dir "cljel"))))
    (is (not (.exists (io/file dir "pom.xml"))))))

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

(deftest no-classpath-dirs-still-requires-entrypoints
  (let [calls (atom [])
        eval-fn (fn [code timeout]
                  (swap! calls conj [code timeout])
                  {:success (not= code (loader/bridge-ready-elisp))})]
    (with-redefs [loader/resolve-elisp-dirs (constantly [])]
      (is (true? (loader/ensure-loaded! eval-fn))))
    (is (= [(loader/bridge-ready-elisp) (loader/load-entrypoints-elisp)]
           (mapv first @calls)))))

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

(deftest eval-with-bridge-evaluates-after-readiness
  (let [calls (atom [])
        eval-fn (fn [code timeout]
                  (swap! calls conj [code timeout])
                  {:success true :result code})]
    (with-redefs [loader/ensure-loaded! (constantly true)]
      (is (= {:success true :result "(+ 1 2)"}
             (loader/eval-with-bridge eval-fn "(+ 1 2)" 9000))))
    (is (= [["(+ 1 2)" 9000]] @calls))))

(deftest eval-with-bridge-remains-retryable
  (let [ready? (atom false)
        calls (atom [])
        eval-fn (fn [code timeout]
                  (swap! calls conj [code timeout])
                  {:success true :result "ok"})]
    (with-redefs [loader/ensure-loaded! (fn [_] @ready?)]
      (is (= {:success false
              :error "Emacs bridge entrypoints failed to load"
              :bridge-unavailable true}
             (loader/eval-with-bridge eval-fn "first" 1000)))
      (reset! ready? true)
      (is (= {:success true :result "ok"}
             (loader/eval-with-bridge eval-fn "second" 2000))))
    (is (= [["second" 2000]] @calls))))

(deftest ensure-loaded-once-latches-success-and-retries-failure
  (let [attempts (atom 0)
        ready? (atom false)]
    (loader/invalidate-ready!)
    (with-redefs [loader/ensure-loaded! (fn [_] (swap! attempts inc) @ready?)]
      (is (false? (loader/ensure-loaded-once! identity)))
      (is (false? (loader/ensure-loaded-once! identity)))
      (is (= 2 @attempts))
      (reset! ready? true)
      (is (true? (loader/ensure-loaded-once! identity)))
      (is (true? (loader/ensure-loaded-once! identity)))
      (is (= 3 @attempts))
      (loader/invalidate-ready!)
      (is (true? (loader/ensure-loaded-once! identity)))
      (is (= 4 @attempts)))
    (loader/invalidate-ready!)))
