(ns loco.constraints.inverse-channeling

  (:require
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :refer :all :as utils]

   [meander.epsilon :as m :refer [match]]
   [clojure.walk :as walk])
  (:import
   [org.chocosolver.solver.variables SetVar IntVar BoolVar Task]))

(def ^:private constraint-name 'inverse-channeling)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/tuple
                       (s/tuple (s/coll-of int-var?) #{'offset} nat-int?)
                       (s/tuple (s/coll-of int-var?) #{'offset} nat-int?)))))

(compile-function compiler constraint-name [*conformed *model]
 (match *conformed
   {:args [[?vars1 _ ?offset1] [?vars2 _ ?offset2]]}
   (.inverseChanneling *model
                       (into-array IntVar ?vars1)
                       (into-array IntVar ?vars2)
                       ?offset1 ?offset2)))

(defn $inverse-channeling
  "Creates an inverse channeling between vars1 and vars2:
  vars1[i-offset2] = j <=> vars2[j-offset1] = i Performs AC if domains are enumerated.
  If not, then it works on bounds without guaranteeing BC
  *(enumerated domains are strongly recommended)

  Beware you should have |vars1| = |vars2|"
  {:choco ["inverseChanneling(IntVar[] vars1, IntVar[] vars2)"
           "inverseChanneling(IntVar[] vars1, IntVar[] vars2, int offset1, int offset2)"]}
  ([vars1 vars2] ($inverse-channeling vars1 0 vars2 0))
  ([vars1 offset1 vars2 offset2]
   {:pre [(= (count vars1) (count vars2))
          (every? sequential? [vars1 vars2])
          (every? nat-int? [offset1 offset2])]}
   (constraint constraint-name
               [[(vec vars1) 'offset  offset1]
                [(vec vars2) 'offset  offset2]]
               compiler)))
