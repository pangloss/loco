(ns loco.constraints.int-value-precede-chain
  (:use loco.constraints.utils)
  (:require
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :as utils]
   [loco.match :refer [match+]]
   [clojure.core.match :refer [match]]
   [clojure.walk :as walk])
  (:import
   [org.chocosolver.solver.variables SetVar IntVar BoolVar Task]))

(def ^:private constraint-name 'int-value-precede-chain)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/or
                       :X-V   (s/tuple
                               (s/coll-of int-var?)
                               (s/coll-of int?))
                       :X-S-T (s/tuple
                               (s/coll-of int-var?)
                               int?
                               int?)))))

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:args [:X-V   [xs vs]]}
           (.intValuePrecedeChain model
                                  (into-array IntVar xs)
                                  (int-array vs))

           {:args [:X-S-T [xs s t]]}
           (.intValuePrecedeChain model (into-array IntVar xs) s t)

           ::s/invalid
           (report-spec-error constraint-name ::compile-spec var-subed-statement))))

;;TODO: find better names for  int-value-precede-chain arguments
(defloco $int-value-precede-chain
  "Creates an intValuePrecedeChain constraint.
  Ensure that, for each pair of V[k] and V[l] of values in V,
  such that k < l, if there exists j such that X[j] = V[l], then,
  there must exist i < j such that X[i] = V[k].

  Creates an intValuePrecedeChain constraint.
  Ensure that if there exists j such that X[j] = T, then,
  there must exist i < j such that X[i] = S."
  {:choco ["intValuePrecedeChain(IntVar[] X, int[] V)"
           "intValuePrecedeChain(IntVar[] X, int S, int T)"]}
  ([xs vs]
   {:pre [(every? int? vs) (sequential? xs)]}
   (constraint constraint-name
               [(vec xs)
                (preserve-consts (vec vs))]
               compiler))

  ([xs s t]
   {:pre [(int? s) (int? t) (sequential? xs)]}
   (constraint constraint-name
               [(vec xs)
                (preserve-consts s)
                (preserve-consts t)]
               compiler)))
