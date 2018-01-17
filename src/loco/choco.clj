#_(ns loco.choco
  loco.core :as core :refer [->choco ->choco* *solver*]
  (:import (org.chocosolver.solver.constraints
            Arithmetic
            Constraint)

           org.chocosolver.solver.Model

           (org.chocosolver.solver.variables
            IntVar
            BoolVar)

           org.chocosolver.solver.constraints.nary.automata.FA.FiniteAutomaton))

;; arithmetic modifiers

#_(defn- $+view
  [x const]
  (let [model (:csolver *solver*)]
    (.intOffsetView model x const)))

#_(defn- $*view
  [x const]
  (let [model (:csolver *solver*)]
    (.intScaleView model x const)))
