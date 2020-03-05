(ns loco.constraints.set.subset-eq

  (:require
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :refer :all :as utils]

   [meander.epsilon :as m :refer [match]]
   [clojure.walk :as walk])
  (:import
   [org.chocosolver.solver.variables SetVar IntVar BoolVar Task]))

(def ^:private constraint-name 'set/subset-eq)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/coll-of set-var?))))

(compile-function
 (match *conformed
   {:args ?sets} (.subsetEq *model (into-array SetVar ?sets))))

(defn $subset-equal
  ;;TODO: fix subset-equal docs
  ;; lawl, choco docs
  "Creates a constraint establishing that sets[i] is a subset of sets[j] if i"
  {:choco "subsetEq(SetVar... sets)"
   :arglists '([set-var ...]
               [set-list])}
  [& sets]
  (match (vec sets)
    [(m/pred vector? ?set-list)]
         (constraint constraint-name
                     (vec ?set-list)
                     compiler)

         ?set-list ($subset-equal ?set-list)))
