(ns hive-emacs.vessel.translator-escaping-test
  "Every caller-supplied string a closed magit, projectile or legacy-memory op
   carries reaches the elisp payload ONLY as a dialect string literal.

   Metamorphic property: building an op with a hostile string S and then
   replacing every occurrence of (string-literal S) by (string-literal P), for
   a fixed benign placeholder P, yields byte-for-byte the payload built with
   P. If any byte of S escaped its literal (raw interpolation, a missing
   escape), the two payloads differ."
  (:require [clojure.string :as str]
            [clojure.test :refer [is]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-vessel.dialect.elisp :as el]
            [hive-emacs.magit.translators :as magit]
            [hive-emacs.projectile.translators :as projectile]
            [hive-emacs.memory.translators :as memory]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def ^:private placeholder "PLACEHOLDER")

(def ^:private builders
  "Label -> (fn [s] payload) for every string-carrying field of every op."
  {:status-dir      #(magit/status-code {:directory %})
   :branches-dir    #(magit/branches-code {:directory %})
   :log-dir         #(magit/log-code {:directory % :count 3})
   :diff-dir        #(magit/diff-code {:directory % :target "all"})
   :stage-dir       #(magit/stage-code {:directory % :files :all})
   :stage-files     #(magit/stage-code {:directory "/r" :files [% "b"]})
   :verify-dir      #(magit/stage-verify-code {:directory % :paths ["a"]})
   :verify-paths    #(magit/stage-verify-code {:directory "/r" :paths [%]})
   :commit-message  #(magit/commit-code {:directory "/r" :message % :all false})
   :commit-dir      #(magit/commit-code {:directory % :message "m" :all true})
   :push-remote     #(magit/push-code {:directory "/r" :set-upstream true :remote %})
   :push-dir        #(magit/push-code {:directory % :set-upstream false :remote nil})
   :pull-dir        #(magit/pull-code {:directory %})
   :fetch-remote    #(magit/fetch-code {:directory "/r" :remote %})
   :fetch-dir       #(magit/fetch-code {:directory % :remote nil})
   :feature-dir     #(magit/feature-branches-code {:directory %})
   :files-pattern   #(projectile/files-code {:pattern %})
   :find-filename   #(projectile/find-code {:filename %})
   :search-pattern  #(projectile/search-code {:pattern %})
   :legacy-project  #(memory/legacy-export-code {:project-id %})})

(def ^:private gen-hostile
  "Strings built to break out of a literal: quotes, backslashes, parens,
   newlines, comments, control characters and random text. Always led by a
   double quote, so the literal never collides with a fixed literal of the
   payload (e.g. \"b\") and every case exercises escaping."
  (gen/fmap #(apply str "\"" %)
            (gen/vector (gen/one-of [(gen/elements ["\"" "\\" ")" "(" "\n" ";" "'" "`" "," "#" "\u0000" "\t" "(kill-emacs)" "\\\")"])
                                     gen/string-alphanumeric])
                        1 8)))

(defn- contained?
  "True when the payload built from S differs from the placeholder payload
   only inside S's string literal(s)."
  [{:keys [label s]}]
  (let [build (get builders label)]
    (= (build placeholder)
       (str/replace (build s) (el/string-literal s) (el/string-literal placeholder)))))

(deftrifecta caller-strings-stay-inside-literals
  contained?
  {:gen (gen/hash-map :label (gen/elements (vec (keys builders)))
                      :s (gen/such-that #(not (str/includes? % placeholder)) gen-hostile))
   :pred true?
   :num-tests 300
   :mutations [["raw-interpolation"
                (fn [{:keys [s]}]
                  ;; What a builder that pasted S in unescaped would produce.
                  (= (str "(f " (el/string-literal placeholder) ")")
                     (str/replace (str "(f \"" s "\")") (el/string-literal s)
                                  (el/string-literal placeholder))))]]
   :assert (fn []
             (is (contained? {:label :push-remote :s "x\") (kill-emacs) (\""}))
             (is (contained? {:label :commit-message :s "a\\\"b\n;c"}))
             (is (= (str "(progn (require 'hive-mcp-magit nil t) (hive-mcp-magit-api-commit "
                         "\"\\\")(kill-emacs)\" nil \"/r\"))")
                    (magit/commit-code {:directory "/r" :message "\")(kill-emacs)" :all false}))
                 "the hostile message is one escaped literal argument"))})
