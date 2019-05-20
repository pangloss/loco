(ns loco.constraints.set.intersection
  (:use loco.constraints.utils)
  (:require
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :as utils]
   [loco.match :refer [match+]]
   [clojure.core.match :refer [match]]
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

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:args [intersection-set [_ sets] [_ bounds-consistent?]]}
           (.intersection model (into-array SetVar sets) intersection-set bounds-consistent?)

           ::s/invalid
           (report-spec-error constraint-name ::compile-spec var-subed-statement))))

(defloco $intersection
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
