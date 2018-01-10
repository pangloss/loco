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



;;LOGIC
(defmethod ->choco* :true
  [_]
  (let [model (:csolver *solver*)]
    (.trueConstraint model)))

(defmethod ->choco* :false
  [_]
  (let [model (:csolver *solver*)]
    (.falseConstraint model)))

(defmethod ->choco* :and
  [{constraints :constraints}]
  (let [constraints (map ->choco constraints)
        model (:csolver *solver*)]
    (.and model (into-array Constraint constraints))))

(defmethod ->choco* :or
  [{constraints :constraints}]
  (let [constraints (map ->choco constraints)
        model (:csolver *solver*)]
    (.or model (into-array Constraint constraints))))

(defmethod ->choco* :not
  [{C :arg}]
  (let [model (:csolver *solver*)]
    (.not model (->choco C))))

(defmethod ->choco* :if
  [{if-this :if then-this :then else-this :else}]
  (let [model (:csolver *solver*)]
    (if-not else-this
      (.ifThen  model
                (->choco if-this)
                (->choco then-this))
      (.ifThenElse model
                   (->choco if-this)
                   (->choco then-this)
                   (->choco else-this)))))

(defmethod ->choco* :reify
  [{C :arg}]
  (let [C (->choco C)
        V (make-bool-var)
        model (:csolver *solver*)]
    (.reification model V C)
    V))

(defmethod ->choco* :regular
  [{:keys [list-of-vars automaton]}]
  (let [list-of-vars (map ->choco-int-var list-of-vars)
        model (:csolver *solver*)]
    (.regular model (into-array IntVar list-of-vars) automaton)))
