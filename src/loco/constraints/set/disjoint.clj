(ns loco.constraints.set.disjoint
  (:use loco.constraints.utils)
  (:require
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :as utils]
   [loco.match :refer [match+]]
   [clojure.core.match :refer [match]]
   [clojure.walk :as walk])
  (:import
   [org.chocosolver.solver.variables SetVar IntVar BoolVar Task]))

(def ^:private constraint-name 'disjoint)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/tuple set-var? set-var?))))

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:args [set1 set2]}
           (.disjoint model set1 set2)

           ::s/invalid
           (report-spec-error constraint-name ::compile-spec var-subed-statement))))

(defn disjoint
  "Creates a constraint stating that the intersection of set1 and set2 should be empty Note that they can be both empty"
  {:choco "disjoint(SetVar set1, SetVar set2)"}
  ([[set1 set2 :as set-pair]]
   {:pre [(= 2 (count set-pair))]}
   (disjoint set1 set2))
  ([set1 set2]
   (constraint constraint-name
               [set1 set2]
               compiler)))
