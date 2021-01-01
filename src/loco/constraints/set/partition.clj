(ns loco.constraints.set.partition

  (:require
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :refer :all :as utils]

   [meander.epsilon :as m :refer [match]]
   [clojure.walk :as walk])
  (:import
   [org.chocosolver.solver.variables SetVar IntVar BoolVar Task]))

(def ^:private constraint-name 'set/partition)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/tuple (s/coll-of set-var?)
                               (s/tuple #{'universe} set-var?)))))

(compile-function compiler constraint-name [*conformed *model]
 (match *conformed
   {:args [?sets ['universe ?universe]]}
   (.partition *model (into-array SetVar ?sets) ?universe)))

(defn $partition
  "Creates a constraint stating that partitions universe into sets: union(sets) = universe intersection(sets) = {}"
  {:choco "partition(SetVar[] sets, SetVar universe)"}
  [universe collection]
  {:pre [(sequential? collection)]}
  (constraint constraint-name
              [collection
               ['universe universe]]
              compiler))
