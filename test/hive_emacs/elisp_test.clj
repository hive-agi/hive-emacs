(ns hive-emacs.elisp-test
  "Contract tests for the CLJ->Elisp argument boundary.

   Every hive-mcp tool handler reaches Emacs through these string builders, so
   an argument that renders into something Emacs cannot read is a broken tool,
   not a cosmetic defect."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [hive-emacs.elisp :as el]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defn- call-args
  "Return the argument text of the generated call to FN-SYM."
  [elisp fn-sym]
  (let [open (str "(" fn-sym " ")
        i (str/index-of elisp open)]
    (when i
      (let [tail (subs elisp (+ i (count open)))]
        (subs tail 0 (str/index-of tail ")"))))))

(deftest booleans-render-as-elisp-truth-values
  (testing "true and false are Clojure literals with no Elisp spelling"
    (let [elisp (el/require-and-call-json 'hive-mcp-cider 'hive-mcp-cider-apropos
                                          "map" true nil)]
      (is (= "\"map\" t nil" (call-args elisp "hive-mcp-cider-apropos"))
          "true must reach Emacs as t"))
    (let [elisp (el/require-and-call-json 'hive-mcp-cider 'hive-mcp-cider-apropos
                                          "map" false nil)]
      (is (= "\"map\" nil nil" (call-args elisp "hive-mcp-cider-apropos"))
          "false must reach Emacs as nil"))))

(deftest scalars-keep-their-elisp-spelling
  (testing "the remaining argument kinds are unchanged by the boolean fix"
    (let [elisp (el/require-and-call-text 'f 'g "s" 42 :kw 'sym nil)]
      (is (= "\"s\" 42 :kw 'sym nil" (call-args elisp "g"))))))

(deftest optional-trailing-args-survive-as-nil
  (testing "an omitted optional arg is an explicit nil, never a dropped slot"
    (let [elisp (el/require-and-call-json 'hive-mcp-cider 'hive-mcp-cider-doc
                                          "map" nil)]
      (is (= "\"map\" nil" (call-args elisp "hive-mcp-cider-doc"))))))
