(ns loco.constraints.all-different-except-0
  (:use loco.constraints.utils)
  (:require
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :as utils]
   [loco.match :refer [match+]]
   [clojure.core.match :refer [match]]
   [clojure.walk :as walk])
  (:import
   [org.chocosolver.solver.variables IntVar]))

(def ^:private constraint-name 'all-different-except-0)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :ints       (s/coll-of int-var?)))

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:ints vars}
           (.allDifferentExcept0 model (->> vars (into-array IntVar)))

           ::s/invalid
           (utils/report-spec-error constraint-name ::compile-spec var-subed-statement))))

(defn distinct-except-0
  "Creates an allDifferent constraint for variables that are not equal to 0.
  There can be multiple variables equal to 0."
  {:choco "allDifferentExcept0(IntVar[] vars)"
   :arglists '([ints-list]
               [& int-vars])}
  [& vars]
  {:pre [(sequential? vars)]}
  (match (vec vars)
         [var-list :guard sequential?]
         (constraint constraint-name
                     (vec var-list)
                     compiler)

         [& var-list]
         (constraint constraint-name
                     (vec var-list)
                     compiler)))

(def all-different-except-0 distinct-except-0)
(reset-meta! (var all-different-except-0) (meta (var distinct-except-0)))
