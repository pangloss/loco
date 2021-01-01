(ns loco.constraints.set.all-disjoint

  (:require
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :refer :all :as utils]

   [meander.epsilon :as m :refer [match]]
   [clojure.walk :as walk])
  (:import
   [org.chocosolver.solver.variables SetVar IntVar BoolVar Task]))

(def ^:private constraint-name 'set/all-disjoint)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/coll-of set-var?))))

(compile-function compiler constraint-name [*conformed *model]
 (match *conformed
   {:args ?sets} (.allDisjoint *model (into-array SetVar ?sets))))

(defn $all-disjoint
  "Creates a constraint stating that the intersection of sets should be empty Note that there can be multiple empty sets"
  {:choco "allDisjoint(SetVar... sets)"}
  [& sets]
  (match (vec sets)
    [(m/pred sequential? ?set-list)] (constraint constraint-name (vec ?set-list) compiler)
    ?set-list                        ($all-disjoint ?set-list)))
