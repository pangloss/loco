(ns loco.constraints.set.symetric

  (:require
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :refer :all :as utils]
   [meander.epsilon :as m :refer [match]]
   [clojure.walk :as walk])
  (:import
   [org.chocosolver.solver.variables SetVar IntVar BoolVar Task]))

(def ^:private constraint-name 'set/symetric)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/tuple (s/coll-of set-var?)
                               (s/tuple #{'offset} nat-int?)))))

(compile-function compiler constraint-name [*conformed *model]
 (match *conformed
   {:args [?sets [_ ?offset]]}
   (.symmetric *model (into-array SetVar ?sets) ?offset)))

(defn $symetric
  "Creates a constraint stating that sets are symmetric sets: x in sets[y] <=> y in sets[x]
  Creates a constraint stating that sets are symmetric sets: x in sets[y-offset] <=> y in sets[x-offset]"
  {:choco ["symmetric(SetVar... sets)"
           "symmetric(SetVar[] sets, int offset)"]
   :arglists '([sets]
               [sets offset]
               [& set ...])}
  [& sets]
  (match (vec sets)
    [(m/pred sequential? ?set-list) (m/pred nat-int? ?offset)]
    (constraint constraint-name
                [(vec ?set-list)
                 ['offset  ?offset]]
                compiler)

    [(m/pred sequential? ?set-list)] ($symetric ?set-list 0)
    ?set-list ($symetric ?set-list 0)))
