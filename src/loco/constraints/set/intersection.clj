(ns loco.constraints.set.intersection

  (:require
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :refer :all :as utils]

   [meander.epsilon :as m :refer [match]]
   [clojure.walk :as walk])
  (:import
   [org.chocosolver.solver.variables SetVar IntVar BoolVar Task]))

(def ^:private constraint-name 'set/intersection)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/tuple
                       set-var?
                       (s/tuple #{'of} (s/coll-of set-var?))
                       (s/tuple #{'bound-consistent} boolean?)))))

(compile-function compiler constraint-name [*conformed *model]
 (match *conformed
   {:args [?intersection-set [_ ?sets] [_ ?bounds-consistent?]]}
   (.intersection *model (into-array SetVar ?sets) ?intersection-set ?bounds-consistent?)))

(defn $intersection
  "Creates a constraint which ensures that the intersection of sets is equal to intersectionSet
  Creates a constraint which ensures that the intersection of sets is equal to intersectionSet"
  {:choco ["intersection(SetVar[] sets, SetVar intersectionSet)"
           "intersection(SetVar[] sets, SetVar intersectionSet, boolean boundConsistent)"]}
  ([intersection-set sets] ($intersection intersection-set sets false))

  ([intersection-set sets bounds-consistent?]
   {:pre [(sequential? sets) (boolean? bounds-consistent?)]}
   (constraint constraint-name
               [intersection-set
                ['of (vec sets)]
                ['bound-consistent bounds-consistent?]]
               compiler)))
