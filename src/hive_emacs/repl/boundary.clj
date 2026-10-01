;; PROPRIETARY - Copyright 2026 BuddhiLW. All Rights Reserved.
;; This file is part of hive-emacs and may not be distributed
;; without explicit written permission.

(ns hive-emacs.repl.boundary
  "The only effectful layer: run a plan through an injected elisp evaluator.

   Every request is issued asynchronously and awaited under a deadline. A Slynk
   request naming a callee the image cannot resolve is never answered, and a
   synchronous wait on it blocks the whole editor; the deadline is what bounds
   that to one failed verb.

   The evaluator is injected, so tests drive this namespace with a recording
   stub and never a live host."
  (:require [clojure.string :as str]
            [hive-dsl.result :as result]
            [hive-emacs.repl.schema :as schema]
            [hive-emacs.repl.translate :as translate]
            [malli.core :as m]))

;;; =============================================================================
;;; Boundary seam (DIP)
;;; =============================================================================

(def ^:dynamic *eval-fn*
  "Elisp evaluation boundary: (f elisp-string timeout-ms) ->
   {:success bool :result any :error any}. Bound to a stub in tests; the addon
   binds it to the host's emacsclient bridge."
  (fn [_elisp _timeout-ms]
    {:success false :error "hive-emacs.repl.boundary/*eval-fn* is unbound"}))

;;; =============================================================================
;;; Lisp literal encoding
;;; =============================================================================

(m/=> lisp-arg [:=> [:cat :any] :string])

(defn lisp-arg
  "VALUE as a Common Lisp literal.

   Clojure nil and false both render as CL NIL, and true as CL T, because CL has
   no distinct boolean false. Symbols are emitted verbatim so a caller may pass
   an already-qualified callee."
  [value]
  (cond
    (nil? value) "cl:nil"
    (true? value) "cl:t"
    (false? value) "cl:nil"
    (string? value) (translate/cl-string value)
    (keyword? value) (str ":" (name value))
    (symbol? value) (str value)
    :else (str value)))

(m/=> call-form [:=> [:cat schema/Call] :string])

(defn call-form
  "CALL as Common Lisp source: the callee applied to its encoded arguments."
  [{:call/keys [rpc args]}]
  (str "(" rpc (when (seq args) (str " " (str/join " " (map lisp-arg args)))) ")"))

(m/=> require-module-form [:=> [:cat :string] :string])

(defn require-module-form
  "Source loading MODULE through the loader's one-argument require-module.

   The loader is reached by string lookup, never a qualified symbol. Every
   condition the load signals is trapped: the form yields CL T on success, or
   the condition's text as a string."
  [module]
  (str "(cl:handler-case"
       " (cl:handler-bind ((cl:warning (cl:lambda (c)"
       " (cl:let ((r (cl:find-restart 'cl:muffle-warning c)))"
       " (cl:when r (cl:invoke-restart r))))))"
       " (cl:funcall (cl:find-symbol \"REQUIRE-MODULE\" \"SLYNK-LOADER\") :" module ")"
       " cl:t)"
       " (cl:condition (c) (cl:princ-to-string c)))"))

;;; =============================================================================
;;; Elisp emission — bounded await
;;; =============================================================================

(m/=> bounded-elisp [:=> [:cat :string pos-int?] :string])

(defn bounded-elisp
  "Elisp evaluating LISP-SOURCE on the current SLY connection, awaiting the
   reply for at most TIMEOUT-MS.

   Yields (:ok VALUE), (:timeout SECS) or (:error MSG). `with-timeout` is what
   keeps the editor responsive when a reply never arrives."
  [lisp-source timeout-ms]
  (let [secs (max 1 (long (Math/ceil (/ (double timeout-ms) 1000.0))))]
    (format
     (str "(let ((hive--done nil) (hive--result nil))"
          "  (condition-case hive--err"
          "      (progn"
          "        (sly-eval-async (car (read-from-string %s))"
          "                        (lambda (v) (setq hive--result (list :ok v) hive--done t))"
          "                        nil)"
          "        (with-timeout (%d (setq hive--result (list :timeout %d)))"
          "          (while (not hive--done) (accept-process-output nil 0.05)))"
          "        hive--result)"
          "    (error (list :error (error-message-string hive--err)))))")
     (pr-str lisp-source) secs secs)))

(m/=> nrepl-value [:=> [:cat :any] :string])

(defn nrepl-value
  "VALUE as an elisp literal for an nREPL request field: integers stay
   integers, everything else is sent as a string."
  [value]
  (if (integer? value)
    (str value)
    (pr-str (if (keyword? value) (name value) (str value)))))

(m/=> nrepl-request-form [:=> [:cat schema/Call] :string])

(defn nrepl-request-form
  "CALL as an elisp list literal: an nREPL request whose op is the part of
   :call/rpc after the slash, each argument keyed by :call/wire-keys. Throws
   when an argument has no wire key."
  [{:call/keys [rpc args wire-keys]}]
  (let [op (subs rpc (inc (str/index-of rpc "/")))
        ks (vec wire-keys)]
    (when (> (count args) (count ks))
      (throw (ex-info "nREPL call has an argument with no wire key"
                      {:rpc rpc :args args :wire-keys ks})))
    (str "(list \"op\" " (pr-str op)
         (apply str (map (fn [k v] (str " " (pr-str k) " " (nrepl-value v))) ks args))
         ")")))

(m/=> nrepl-bounded-elisp [:=> [:cat :string pos-int?] :string])

(defn nrepl-bounded-elisp
  "Elisp sending REQUEST-FORM on the current CIDER connection and awaiting its
   \"done\" status for at most TIMEOUT-MS.

   Yields (:ok ALIST), (:timeout SECS) or (:error MSG). The alist merges every
   response: string fields concatenate, others keep their last value."
  [request-form timeout-ms]
  (let [secs (max 1 (long (Math/ceil (/ (double timeout-ms) 1000.0))))]
    (format
     (str "(let ((hive--done nil) (hive--acc nil) (hive--result nil))"
          "  (condition-case hive--err"
          "      (progn"
          "        (cider-nrepl-send-request %s"
          "          (lambda (r) (push r hive--acc)"
          "            (when (member \"done\" (nrepl-dict-get r \"status\")) (setq hive--done t)))"
          "          (cider-current-repl) nil)"
          "        (with-timeout (%d (setq hive--result (list :timeout %d)))"
          "          (while (not hive--done) (accept-process-output nil 0.05)))"
          "        (or hive--result"
          "            (let (hive--m)"
          "              (dolist (r (nreverse hive--acc))"
          "                (nrepl-dict-map"
          "                 (lambda (k v)"
          "                   (unless (member k '(\"id\" \"session\" \"status\"))"
          "                     (let ((cell (assoc k hive--m)))"
          "                       (cond ((and cell (stringp v) (stringp (cdr cell)))"
          "                              (setcdr cell (concat (cdr cell) v)))"
          "                             (cell (setcdr cell v))"
          "                             (t (push (cons k v) hive--m))))))"
          "                 r))"
          "              (list :ok (nreverse hive--m)))))"
          "    (error (list :error (error-message-string hive--err)))))")
     request-form secs secs)))

(defmulti call-elisp
  "Elisp issuing CALL on its transport under the call's deadline."
  (fn [call] (:call/transport call :sly)))

(defmethod call-elisp :sly
  [call]
  (bounded-elisp (call-form call) (:call/timeout-ms call)))

(defmethod call-elisp :nrepl
  [call]
  (nrepl-bounded-elisp (nrepl-request-form call) (:call/timeout-ms call)))

;;; =============================================================================
;;; Execution
;;; =============================================================================

(defn- eval-elisp
  [elisp timeout-ms]
  (let [{:keys [success result error]} (*eval-fn* elisp timeout-ms)]
    (if success
      (result/ok result)
      (result/err :transport {:fail/kind :transport
                              :fail/message (str error)}))))

(m/=> ensure-prelude [:=> [:cat [:vector :string] pos-int?] :any])

(defn ensure-prelude
  "Load each module in MODULES, in order. Returns ok with the module vector, or
   the first transport failure. A module that reports it was not provided is
   not an error: the loader still compiles and loads the file."
  [modules timeout-ms]
  (reduce (fn [acc module]
            (let [r (eval-elisp (bounded-elisp (require-module-form module) timeout-ms)
                                timeout-ms)]
              (if (result/ok? r) acc (reduced r))))
          (result/ok (vec modules))
          modules))

(m/=> execute [:=> [:cat schema/Plan] :any])

(defn execute
  "Run PLAN: ensure its prelude, then issue its call on its transport under the
   call's deadline.

   Returns a Result whose ok value is {:shape :value}, so a caller can decode
   without re-deriving which op produced it."
  [{:plan/keys [prelude call]}]
  (let [timeout-ms (:call/timeout-ms call)]
    (result/let-ok [_ (ensure-prelude (vec prelude) timeout-ms)
                    v (eval-elisp (call-elisp call) timeout-ms)]
                   (result/ok {:shape (:call/shape call) :value v}))))

(m/=> run [:=> [:cat schema/Request] :any])

(defn run
  "Translate REQUEST and execute it. The one entry point a verb handler needs."
  [request]
  (result/let-ok [plan (translate/plan request)]
                 (execute plan)))
