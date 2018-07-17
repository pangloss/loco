(ns loco.constraints.set.sum-elements
  (:use loco.constraints.utils)
  (:require
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :as utils]
   [loco.match :refer [match+]]
   [clojure.core.match :refer [match]]
   [clojure.walk :as walk])
  (:import
   [org.chocosolver.solver.variables SetVar IntVar BoolVar Task]))

(def ^:private constraint-name 'sum-elements)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/tuple int-var?
                               (s/tuple #{'indices} set-var?)
                               (s/tuple #{'weights} (s/coll-of int?))
                               (s/tuple #{'offset} nat-int?)))))

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:args [result-var [_ indices-set] [_ weights] [_ offset]]}
           (.sumElements model indices-set (int-array weights) offset result-var)

           ::s/invalid
           (report-spec-error constraint-name ::compile-spec var-subed-statement))))

;;TODO: can do partial for sum-elements
(defn $sum-elements
  "Creates a constraint summing weights given by a set of indices:
  sum{weights[i-offset] | i in indices} = sum Also ensures that
  elements in indices belong to [offset, offset+weights.length-1]

  Creates a constraint summing weights given by a set of indices:
  sum{weights[i] | i in indices} = sum Also ensures that elements in
  indices belong to [0, weights.length-1]"
  {:choco ["sumElements(SetVar indices, int[] weights, IntVar sum)"
           "sumElements(SetVar indices, int[] weights, int offset, IntVar sum)"]}
  ([sum-var indices-set weights] ($sum-elements sum-var indices-set weights 0))
  ([sum-var indices-set weights offset]
   {:pre [(sequential? weights)
          (every? int? weights)
          (nat-int? offset)]}
   (constraint constraint-name
               [sum-var
                ['indices indices-set]
                ['weights (preserve-consts (vec weights))]
                ['offset  (preserve-consts offset)]])))
