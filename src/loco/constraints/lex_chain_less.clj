(ns loco.constraints.lex-chain-less
  (:require
   [loco.utils :refer [p c]]
   [loco.constraints.utils :refer :all :as utils]
   [clojure.spec.alpha :as s]
   [meander.epsilon :as m :refer [match]]
   [clojure.walk :as walk])
  (:import
   [org.chocosolver.solver.variables SetVar IntVar BoolVar Task]))

(def ^:private constraint-name 'lex-chain-less)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/coll-of (s/coll-of int-var?)))))

(compile-function compiler constraint-name [*conformed *model]
 (match *conformed
   {:args ?vars-vectors}
   (.lexChainLessEq *model
                    (->> ?vars-vectors
                         (map (p into-array IntVar))
                         into-array))))

;;TODO: lex-chain-less is sort? make alias if so
(defn $lex-chain-less
  "Creates a lexChainLess constraint.
  For each pair of consecutive vectors varsi and varsi+1 of the vars collection
  varsi is lexicographically strictly less than than varsi+1"
  {:choco "lexChainLess(IntVar[]... vars)"
   :arglists '([int-var-vectors] [int-var-vector...])}
  [& more]
  (match (vec more)
    [(m/pred (every-pred sequential? (p every? sequential?)) ?int-vars-vectors)]
    (constraint constraint-name
                (mapv vec ?int-vars-vectors)
                compiler)

    ?int-vars-vectors
    (constraint constraint-name
                (mapv vec ?int-vars-vectors)
                compiler)))
