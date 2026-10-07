(ns hive-emacs.no-prompt-test
  "Every agent eval runs under the elisp prompt refuser, and a refusal
   reaches the caller as a flagged failure. The emacsclient boundary is a
   recording transport installed with client/set-transport!."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is use-fixtures]]
            [clojure.test.check.generators :as gen]
            [hive-emacs.client :as client]
            [hive-emacs.no-prompt :as no-prompt]
            [hive-test.trifecta :refer [deftrifecta]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defn- wrapped?
  "True when OUT hands its form to hive-mcp-no-prompt-call and closes on a
   line of its own."
  [out]
  (and (str/starts-with? out "(progn (require 'hive-mcp-no-prompt nil t)")
       (str/includes? out "#'hive-mcp-no-prompt-call")
       (str/ends-with? out "\n)))")))

(deftrifecta refuse-prompts
  hive-emacs.no-prompt/refuse-prompts
  {:golden-path "test/golden/hive_emacs/no_prompt/refuse_prompts.edn"
   :cases {:atom             "t"
           :call             "(+ 1 2)"
           :find-file        "(find-file \"/tmp/a.clj\")"
           :trailing-comment "(message \"hi\") ; a comment"
           :empty            ""}
   :gen gen/string-ascii
   :pred wrapped?
   :num-tests 100
   :mutations [["unwrapped" identity]
               ["comment-swallows-parens"
                (fn [code]
                  (str "(progn (require 'hive-mcp-no-prompt nil t)"
                       " (funcall (if (fboundp 'hive-mcp-no-prompt-call) #'hive-mcp-no-prompt-call #'funcall)"
                       " (lambda () " code ")))"))]]})

(def ^:private refusal-stderr
  "*ERROR*: Interactive prompt refused: \"Emacs asked: Install now? (y or n); refused for agent calls\"\n")

(defn- flag-consistent?
  "True when M is a failure whose :prompt-refused flag matches its text."
  [m]
  (and (false? (:success m))
       (string? (:error m))
       (= (boolean (:prompt-refused m))
          (str/includes? (:error m) no-prompt/refusal-marker))))

(deftrifecta failure-response
  hive-emacs.no-prompt/failure-response
  {:golden-path "test/golden/hive_emacs/no_prompt/failure_response.edn"
   :cases {:plain-error "boom\n"
           :refusal     refusal-stderr
           :blank       ""
           :nil-stderr  nil}
   :gen (gen/one-of [gen/string-ascii
                     (gen/fmap #(str % "refused for agent calls") gen/string-ascii)])
   :pred flag-consistent?
   :num-tests 100
   :mutations [["never-flags" (fn [err] {:success false :error (str/trim (str err))})]
               ["always-flags" (fn [err] {:success false :error (str/trim (str err))
                                          :prompt-refused true})]]})

(defn- isolate-client
  "Closed breaker and a transport that refuses to spawn emacsclient unless
   the test installs a recording one."
  [f]
  (client/reset-circuit-breaker!)
  (let [previous (client/set-transport!
                  (fn [argv] (throw (ex-info "test reached a live emacsclient" {:argv argv}))))]
    (try
      (f)
      (finally
        (client/set-transport! previous)
        (client/shutdown-executor!)
        (client/reset-circuit-breaker!)))))

(use-fixtures :each isolate-client)

(deftest every-eval-reaches-emacs-wrapped-in-the-refuser
  (let [argvs (atom [])
        code  "(find-file \"/tmp/x.clj\")"]
    (client/set-transport! (fn [argv] (swap! argvs conj argv) {:exit 0 :out "t\n" :err ""}))
    (is (:success (client/eval-elisp code)))
    (is (:success (client/eval-elisp-with-timeout code 1000)))
    (is (= 2 (count @argvs)))
    (is (every? #(= (no-prompt/refuse-prompts code) (last %)) @argvs) (pr-str @argvs))))

(deftest a-refused-prompt-is-a-flagged-failure-not-a-hang
  (client/set-transport! (fn [_] {:exit 1 :out "" :err refusal-stderr}))
  (let [r (client/eval-elisp-with-timeout "(find-file \"/tmp/x.clj\")" 1000)]
    (is (false? (:success r)))
    (is (true? (:prompt-refused r)))
    (is (not (:timed-out r)))
    (is (str/includes? (:error r) "Emacs asked: Install now? (y or n); refused for agent calls"))
    (is (= :closed (:state (client/circuit-breaker-state)))
        "a refused prompt is not daemon death")))
