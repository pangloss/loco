(ns loco.constraints.set.partition
  (:use loco.constraints.utils)
  (:require
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :as utils]
   [loco.match :refer [match+]]
   [clojure.core.match :refer [match]]
   [clojure.walk :as walk])
  (:import
   [org.chocosolver.solver.variables SetVar IntVar BoolVar Task]))

(def ^:private constraint-name 'set/partition)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/tuple (s/coll-of set-var?)
                               (s/tuple #{'universe} set-var?)))))

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:args [sets ['universe universe]]}
           (.partition model (into-array SetVar sets) universe)

           ::s/invalid
           (report-spec-error constraint-name ::compile-spec var-subed-statement))))

(defloco $partition
  "Creates a constraint stating that partitions universe into sets: union(sets) = universe intersection(sets) = {}"
  {:choco "partition(SetVar[] sets, SetVar universe)"}
  [universe collection]
  {:pre [(sequential? collection)]}
  (constraint constraint-name
              [collection
               ['universe universe]]
              compiler))
