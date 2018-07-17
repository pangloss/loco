(ns loco.constraints.sort
  (:use loco.constraints.utils)
  (:require
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :as utils]
   [loco.match :refer [match+]]
   [clojure.core.match :refer [match]]
   [clojure.walk :as walk])
  (:import
   [org.chocosolver.solver.variables SetVar IntVar BoolVar Task]))

(def ^:private constraint-name 'sort)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/tuple
                       (s/coll-of int-var?)
                       (s/coll-of int-var?)))))

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:args [vars sorted-vars]}
           (.sort model
                  (into-array IntVar vars)
                  (into-array IntVar sorted-vars))

           ::s/invalid
           (report-spec-error constraint-name ::compile-spec var-subed-statement))))

(defn $sort
  "Creates a sort constraint which ensures that the variables of
  sorted-vars correspond to the variables of vars according to a
  permutation. The variables of sortedVars are also sorted in increasing
  order.

  For example:
  - X= (4,2,1,3)
  - Y= (1,2,3,4)"
  {:choco "sort(IntVar[] vars, IntVar[] sortedVars)"}
  [vars sorted-vars]
  {:pre [(sequential? vars) (sequential? sorted-vars)]}
  (constraint constraint-name
              [(vec vars) (vec sorted-vars)]
              compiler))
