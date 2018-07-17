(ns loco.constraints.set.all-disjoint
  (:use loco.constraints.utils)
  (:require
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :as utils]
   [loco.match :refer [match+]]
   [clojure.core.match :refer [match]]
   [clojure.walk :as walk])
  (:import
   [org.chocosolver.solver.variables SetVar IntVar BoolVar Task]))

(def ^:private constraint-name 'all-disjoint)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/coll-of set-var?))))

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:args sets}
           (.allDisjoint model (into-array SetVar sets))

           ::s/invalid
           (report-spec-error constraint-name ::compile-spec var-subed-statement))))

(defloco $all-disjoint
  "Creates a constraint stating that the intersection of sets should be empty Note that there can be multiple empty sets"
  {:choco "allDisjoint(SetVar... sets)"}
  [& sets]
  (match (vec sets)
         [set-list :guard sequential?] (constraint constraint-name (vec set-list) compiler)
         set-list ($all-disjoint set-list)))
