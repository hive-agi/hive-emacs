(ns ^:typed.clojure hive-emacs.typed-scoring
  "Typed-Clojure projection of the canonical Malli score schemas."
  (:require [hive-emacs.daemon-scoring :as scoring]
            [hive-emacs.schema]
            [hive-spi.schema.typed-ann :as typed-schema]
            [typed.clojure :as t]))

(typed-schema/defalias-schema ScoreInput :hive-emacs/score-input)
(typed-schema/defalias-schema ScoreSnapshot :hive-emacs/score-snapshot)
(typed-schema/defalias-schema HealthScore :hive-emacs/health-score)
(typed-schema/defalias-schema HealthLevel :hive-emacs/health-level)

(t/ann hive-emacs.daemon-scoring/score-snapshot
       [ScoreInput :-> ScoreSnapshot])
(t/ann hive-emacs.daemon-scoring/health-level
       [(t/Nilable HealthScore) :-> HealthLevel])

(t/ann checked-score-snapshot [ScoreInput :-> ScoreSnapshot])
(defn checked-score-snapshot
  "Statically checked adapter over the runtime Malli-validated function."
  [input]
  (scoring/score-snapshot input))
