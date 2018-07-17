(ns loco.constraints.lex-chain-less
  (:use loco.constraints.utils)
  (:require
   [loco.utils :refer [p c]]
   [clojure.spec.alpha :as s]
   [loco.match :refer [match+]]
   [clojure.core.match :refer [match]]
   [clojure.walk :as walk])
  (:import
   [org.chocosolver.solver.variables SetVar IntVar BoolVar Task]))

(def ^:private constraint-name 'lex-chain-less)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/coll-of (s/coll-of int-var?)))))

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:args vars-vectors}
           (.lexChainLessEq model
                            (->> vars-vectors
                                 (map (p into-array IntVar))
                                 into-array))

           ::s/invalid
           (report-spec-error constraint-name ::compile-spec var-subed-statement))))

;;TODO: lex-chain-less is sort? make alias if so
(defloco $lex-chain-less
  "Creates a lexChainLess constraint.
  For each pair of consecutive vectors varsi and varsi+1 of the vars collection
  varsi is lexicographically strictly less than than varsi+1"
  {:choco "lexChainLess(IntVar[]... vars)"
   :arglists '([int-var-vectors] [int-var-vector...])}
  [& more]
  (match+
   (vec more)
   [int-vars-vectors]
   :guard [int-vars-vectors [sequential? (p every? sequential?)]]
   (constraint constraint-name
               (mapv vec int-vars-vectors)
               compiler)

   int-vars-vectors
   (constraint constraint-name
               (mapv vec int-vars-vectors)
               compiler)))
