(ns loco.constraints.set.disjoint

  (:require
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :refer :all :as utils]

   [meander.epsilon :as m :refer [match]]
   [clojure.walk :as walk])
  (:import
   [org.chocosolver.solver.variables SetVar IntVar BoolVar Task]))

(def ^:private constraint-name 'set/disjoint)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/tuple set-var? set-var?))))

(compile-function compiler constraint-name [*conformed *model]
 (match *conformed
   {:args [?set1 ?set2]} (.disjoint *model ?set1 ?set2)))

(defn $disjoint
  "Creates a constraint stating that the intersection of set1 and set2 should be empty Note that they can be both empty"
  {:choco "disjoint(SetVar set1, SetVar set2)"}
  ([[set1 set2 :as set-pair]]
   {:pre [(= 2 (count set-pair))]}
   ($disjoint set1 set2))
  ([set1 set2]
   (constraint constraint-name
               [set1 set2]
               compiler)))
