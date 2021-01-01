(ns loco.constraints.set.not-empty

  (:require
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :refer :all :as utils]
   [meander.epsilon :as m :refer [match]]
   [clojure.walk :as walk])
  (:import
   [org.chocosolver.solver.variables SetVar IntVar BoolVar Task]))

(def ^:private constraint-name 'set/not-empty)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       set-var?))

(compile-function compiler constraint-name [*conformed *model]
 (match *conformed
   {:args ?set-var} (.notEmpty *model ?set-var)))

(defn $not-empty
  "Creates a constraint preventing set to be empty"
  {:choco "notEmpty(SetVar set)"}
  [set-var]
  (constraint constraint-name set-var compiler))
