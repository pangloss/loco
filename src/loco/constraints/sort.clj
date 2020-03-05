(ns loco.constraints.sort

  (:require
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :refer :all :as utils]

   [meander.epsilon :as m :refer [match]]
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

(compile-function
 (match *conformed
   {:args [?vars ?sorted-vars]}
   (.sort *model
          (into-array IntVar ?vars)
          (into-array IntVar ?sorted-vars))))

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
