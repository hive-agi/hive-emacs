(ns hive-emacs.daemon-ports
  "Compatibility facade over hive-emacs.runtime-ports."
  (:require [hive-emacs.runtime-ports :as runtime]))

(def configure! runtime/configure!)
(def clear! runtime/clear!)
(def snapshot runtime/snapshot)
(def ping! runtime/ping!)
(def emit! runtime/emit!)
