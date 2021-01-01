(ns loco.constraints.set.inverse

  (:require
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :refer :all :as utils]

   [meander.epsilon :as m :refer [match]]
   [clojure.walk :as walk])
  (:import
   [org.chocosolver.solver.variables SetVar IntVar BoolVar Task]))

(def ^:private constraint-name 'set/inverse)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/tuple
                       (s/tuple #{'inverse-sets} (s/coll-of set-var?) #{'offset} nat-int?)
                       (s/tuple #{'sets} (s/coll-of int-var?) #{'offset} nat-int?)))))

(compile-function compiler constraint-name [*conformed *model]
 (match *conformed
   {:args [[_ ?inverse-sets _ ?offset-inverse-set] [_ ?sets _ ?offset-set]]}
   (.inverseSet *model
                (into-array SetVar ?sets)
                (into-array SetVar ?inverse-sets)
                ?offset-set
                ?offset-inverse-set)))

(defn $inverse
  "Creates a constraint stating that : x in sets[y-offset1] <=> y in invSets[x-offset2]"
  {:choco ["inverseSet(SetVar[] sets, SetVar[] invSets, int offset1, int offset2)"]}
  ([sets offset-set inverse-sets offset-invsere-set]
   {:pre [(integer? offset-invsere-set)
          (integer? offset-set)
          (sequential? inverse-sets)
          (sequential? sets)]}
   (constraint constraint-name
               [['inverse-sets (vec inverse-sets)
                 'offset  offset-invsere-set]
                ['sets (vec sets)
                 'offset  offset-set]]
               compiler)))
