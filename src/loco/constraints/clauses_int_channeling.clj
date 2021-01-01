(ns loco.constraints.clauses-int-channeling
  (:require
   [meander.epsilon :as m :refer [match]]
   [clojure.spec.alpha :as s]
   [clojure.walk :as walk]
   [loco.constraints.utils :refer :all :as utils]
   [loco.utils :refer [p]]
   )
  (:import
   [org.chocosolver.solver.variables SetVar IntVar BoolVar]))

(def ^:private constraint-name 'clauses-int-channeling)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/tuple
                       ::utils/int-var?
                       (s/tuple #{'e-vars} ::utils/coll-boolvar?)
                       (s/tuple #{'l-vars} ::utils/coll-boolvar?)
                       ))))

(compile-function compiler constraint-name [*conformed *model]
 (match *conformed
   {:args [?int-var [_ ?e-vars] [_ ?l-vars]]}
   (.clausesIntChanneling *model
                          ?int-var
                          (into-array BoolVar ?e-vars)
                          (into-array BoolVar ?l-vars))))

;;TODO: do the int domain validation in the model/compile step
;;the distance of the LB and UB of int-var needs to be equal to (count e-vars)
(defn $clauses-int-channeling
  "Creates an channeling constraint between an integer variable and a
  set of clauses. Link each value from the domain of var to two
  boolean variable: one reifies the equality to the i^th value of the
  variable domain, the other reifies the less-or-equality to the i^th
  value of the variable domain.

  Contract: eVars.lenght == lVars.length == var.getUB() - var.getLB() + 1
  Contract: var is not a boolean variable"
  {:choco "clausesIntChanneling(IntVar var, BoolVar[] eVars, BoolVar[] lVars)"}
  [var e-vars l-vars]
  {:pre [(every? sequential? [e-vars l-vars]) (= (count e-vars) (count l-vars))]}
  (constraint constraint-name
              [var
               ['e-vars (vec e-vars)]
               ['l-vars (vec l-vars)]]
              compiler))
