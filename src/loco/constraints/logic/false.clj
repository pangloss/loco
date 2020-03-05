(ns loco.constraints.logic.false
  (:require
   [meander.epsilon :as m :refer [match]]
   [clojure.spec.alpha :as s]
   [clojure.walk :as walk]
   [loco.constraints.utils :refer :all :as utils]
   )
  (:import
   [org.chocosolver.solver.variables IntVar BoolVar]))

(def ^:private constraint-name 'false)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args (s/spec
                #{nil})))

(compile-function
 (match *conformed
   {:args _}
   (.falseConstraint *model)))

(defn $false
  "Constraint that is always false"
  {:choco "false()"}
  []
  (constraint constraint-name nil compiler))

;;(defn $false [] (constraint 'false nil identity))
