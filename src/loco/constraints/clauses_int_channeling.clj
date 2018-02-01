(in-ns 'loco.constraints)
(ns loco.constraints.clauses-int-channeling
  (:use loco.constraints.utils)
  (:require
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :as utils]
   [loco.match :refer [match+]]
   [clojure.core.match :refer [match]]
   [clojure.walk :as walk])
  (:import
   [org.chocosolver.solver.variables SetVar IntVar BoolVar]))

(def ^:private constraint-name 'clauses-int-channeling)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/tuple
                       (s/tuple #{'int-var} int-var?)
                       (s/tuple #{'e-vars} (s/coll-of bool-var?))
                       (s/tuple #{'l-vars} (s/coll-of bool-var?))
                       ))))

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:args [[_ int-var] [_ e-vars] [_ l-vars]]}
           (.clausesIntChanneling model
                                  int-var
                                  (into-array BoolVar e-vars)
                                  (into-array BoolVar l-vars))
           ::s/invalid
           (report-spec-error constraint-name ::compile-spec var-subed-statement))))

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
  [int-var e-vars l-vars]
  {:pre [(every? sequential? [e-vars l-vars]) (= (count e-vars) (count l-vars))]}
  (constraint constraint-name
              [['int-var int-var]
               ['e-vars (vec e-vars)]
               ['l-vars (vec l-vars)]]
              compiler))
