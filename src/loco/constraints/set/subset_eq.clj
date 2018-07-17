(ns loco.constraints.set.subset-eq
  (:use loco.constraints.utils)
  (:require
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :as utils]
   [loco.match :refer [match+]]
   [clojure.core.match :refer [match]]
   [clojure.walk :as walk])
  (:import
   [org.chocosolver.solver.variables SetVar IntVar BoolVar Task]))

(def ^:private constraint-name 'subset-eq)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/coll-of set-var?))))

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:args sets}
           (.subsetEq model (into-array SetVar sets))

           ::s/invalid
           (report-spec-error constraint-name ::compile-spec var-subed-statement))))

(defloco $subset-equal
  ;;TODO: fix subset-equal docs
  ;; lawl, choco docs
  "Creates a constraint establishing that sets[i] is a subset of sets[j] if i"
  {:choco "subsetEq(SetVar... sets)"
   :arglists '([set-var ...]
               [set-list])}
  [& sets]
  (match (vec sets)
         [set-list :guard vector?]
         (constraint constraint-name
                     (vec set-list)
                     compiler)

         set-list ($subset-equal set-list)))
