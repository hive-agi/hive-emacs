(ns hive-emacs.root-tool-contract-test
  (:require [hive-addon.tool-contract.test :refer [deftest-root-tools]]
            [hive-addon.protocol :as proto]
            [hive-emacs.addon :as a]
            [hive-emacs.bridge-loader :as bridge]
            [hive-emacs.client :as client]))

;; Tools are only exposed once the addon is active (proto/tools is [] before
;; initialize!), so initialize with stubbed bridge/client, run the contract
;; over the live tool defs, then shut the addon down again.
(deftest-root-tools root-tools
  (with-redefs [bridge/ensure-loaded! (constantly false)
                client/emacs-running? (constantly true)]
    (let [instance (a/addon-ctor {})]
      (try
        (proto/initialize! instance {})
        (proto/tools instance)
        (finally
          (proto/shutdown! instance))))))
