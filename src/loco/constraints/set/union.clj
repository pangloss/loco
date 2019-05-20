(ns loco.constraints.set.union
  (:use loco.constraints.utils)
  (:require
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :as utils]
   [loco.match :refer [match+]]
   [clojure.core.match :refer [match]]
   [clojure.walk :as walk])
  (:import
   [org.chocosolver.solver.variables SetVar IntVar BoolVar Task]))

(def ^:private constraint-name 'set/union)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/or
                       :ints (s/tuple
                              set-var?
                              (s/tuple #{'of} (s/coll-of int-var?)))

                       :sets (s/tuple
                              set-var?
                              (s/tuple #{'of} (s/coll-of set-var?)))))))

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:args [(:or :ints :sets) [union-set [_ vars]]]}
           (.union model (into-array vars) union-set)

           ::s/invalid
           (report-spec-error constraint-name ::compile-spec var-subed-statement))))

;; [:union [union-set [:of (ints :guard (p every? lookup-int-var?))]]]
;; (.union model
;;         (into-array IntVar (map lookup-var ints))
;;         (lookup-var union-set))

(defloco $union
  "Creates a constraint ensuring that union is exactly the union of values taken by ints
  Creates a constraint which ensures that the union of sets is equal to unionSet"
  {:choco ["union(IntVar[] ints, SetVar union)"
           "union(SetVar[] sets, SetVar unionSet)"]}
  [union-set collection]
  {:pre [(sequential? collection)]}
  (constraint constraint-name
              [union-set
               ['of (vec collection)]]
              compiler))
