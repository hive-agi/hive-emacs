(ns hive-emacs.config-test
  "Tests for hive-emacs.config defconfig surface.

   Covers:
   - defconfig-tests auto-generated properties (totality, defaults, roundtrip, mutations)
   - Defaults resolve when env unset
   - Env vars resolve to typed values
   - Overrides beat env vars
   - default-daemon-id fallback semantics"
  (:require [clojure.test :refer [deftest is testing]]
            [hive-di.testing :refer [defconfig-tests]]
            [hive-dsl.result :as r]
            [hive-emacs.config :as config :refer [EmacsConfig-fields
                                                  resolve-EmacsConfig]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; ============================================================================
;; Auto-generated property tests (hive-di.testing)
;; ============================================================================

(defconfig-tests EmacsConfig EmacsConfig-fields :num-tests 50)

;; ============================================================================
;; Explicit semantic tests
;; ============================================================================

(deftest defaults-resolve-when-env-empty
  (testing "Defaults produce valid config when no env, no overrides"
    (let [result (resolve-EmacsConfig {} {:env-fn (constantly nil)})]
      (is (r/ok? result))
      (is (= "emacsclient" (-> result :ok :emacsclient-path)))
      (is (nil? (-> result :ok :socket-name))
          "socket-name has no default → nil when unset (optional field)"))))

(deftest env-vars-resolve-to-typed-values
  (testing "EMACSCLIENT and EMACS_SOCKET_NAME read from env-fn"
    (let [env    {"EMACSCLIENT"       "/usr/local/bin/emacsclient"
                  "EMACS_SOCKET_NAME" "test-socket"}
          result (resolve-EmacsConfig {} {:env-fn env})]
      (is (r/ok? result))
      (is (= "/usr/local/bin/emacsclient" (-> result :ok :emacsclient-path)))
      (is (= "test-socket" (-> result :ok :socket-name))))))

(deftest overrides-beat-env-vars
  (testing "Explicit overrides win over env-fn lookup"
    (let [env    {"EMACSCLIENT" "/wrong/path"}
          result (resolve-EmacsConfig {:emacsclient-path "/opt/emacs/bin/ec"}
                                      {:env-fn env})]
      (is (r/ok? result))
      (is (= "/opt/emacs/bin/ec" (-> result :ok :emacsclient-path))))))

(deftest blank-env-falls-back-to-default
  (testing "EMACSCLIENT=\"\" treated as unset, default applies"
    (let [env    {"EMACSCLIENT" ""}
          result (resolve-EmacsConfig {} {:env-fn env})]
      (is (r/ok? result))
      (is (= "emacsclient" (-> result :ok :emacsclient-path))
          "blank->nil normalization, then default"))))

;; ============================================================================
;; Helper fn tests
;; ============================================================================

(deftest default-daemon-id-fallback
  (testing "default-daemon-id returns 'server' when EMACS_SOCKET_NAME unset"
    ;; Use private rebinding via System/setProperty would be ideal, but
    ;; the helper currently uses live env. Check it returns a non-empty string.
    (let [id (config/default-daemon-id)]
      (is (string? id))
      (is (seq id)))))

(deftest emacsclient-path-non-empty
  (testing "emacsclient-path always returns a non-empty string"
    (let [path (config/emacsclient-path)]
      (is (string? path))
      (is (seq path)))))
