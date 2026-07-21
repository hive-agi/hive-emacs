(ns hive-emacs.init-test
  (:require [clojure.test :refer [deftest is]]
            [hive-addon.protocol :as addon]
            [hive-emacs.addon :as canonical]
            [hive-emacs.init :as init]))

(deftest compatibility-entrypoints-return-uninitialized-addons
  (doseq [instance [(init/addon-ctor {})
                    (init/init-as-addon!)
                    (init/init-as-addon! {})
                    (init/->emacs-addon)]]
    (is (addon/addon? instance))
    (is (= canonical/addon-id-value (addon/addon-id instance)))
    (is (= :down (:status (addon/health instance))))))
