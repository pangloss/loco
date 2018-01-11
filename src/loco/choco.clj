(ns loco.choco
  loco.core :as core :refer [->choco ->choco* *solver*]
  (:import (org.chocosolver.solver.constraints
            Arithmetic
            Constraint)

           org.chocosolver.solver.Model

           (org.chocosolver.solver.variables
            IntVar
            BoolVar)

           org.chocosolver.solver.constraints.nary.automata.FA.FiniteAutomaton))

;; arithmetic

(defn- $+view
  [x const]
  (let [model (:csolver *solver*)]
    (.intOffsetView model x const)))

(defn- $*view
  [x const]
  (let [model (:csolver *solver*)]
    (.intScaleView model x const)))

(defmethod ->choco* :regular
  [{:keys [list-of-vars automaton]}]
  (let [list-of-vars (map ->choco-int-var list-of-vars)
        model (:csolver *solver*)]
    (.regular model (into-array IntVar list-of-vars) automaton)))
