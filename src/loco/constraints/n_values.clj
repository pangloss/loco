(ns loco.constraints.n-values

  (:require
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :refer :all :as utils]

   [meander.epsilon :as m :refer [match]]
   [clojure.walk :as walk])
  (:import
   [org.chocosolver.solver.variables SetVar IntVar BoolVar Task]))

(def ^:private constraint-name 'n-values)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/tuple (s/coll-of int-var?) int-var?))))

(compile-function compiler constraint-name [*conformed *model]
 (match *conformed
   {:args [?vars ?n-values]}
   (.nValues *model (into-array IntVar ?vars) ?n-values)))

(defn $n-values
  "Creates an nValue constraint. Let N be the number of distinct values
  assigned to the variables of the vars collection. Enforce condition
  N = nValues to hold."
  {:choco "nValues(IntVar[] vars, IntVar nValues)"}
  [vars n-values]
  {:pre [(sequential? vars)]}
  (constraint constraint-name
              [(vec vars) n-values]
              compiler))
