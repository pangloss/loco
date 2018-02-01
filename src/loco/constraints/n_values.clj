(in-ns 'loco.constraints)
(ns loco.constraints.n-values
  (:use loco.constraints.utils)
  (:require
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :as utils]
   [loco.match :refer [match+]]
   [clojure.core.match :refer [match]]
   [clojure.walk :as walk])
  (:import
   [org.chocosolver.solver.variables SetVar IntVar BoolVar Task]))

(def ^:private constraint-name 'n-values)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/tuple (s/coll-of int-var?) int-var?))))

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:args [vars n-values]}
           (.nValues model (into-array IntVar vars) n-values)

           ::s/invalid
           (report-spec-error constraint-name ::compile-spec var-subed-statement))))

(defn $n-values
  "Creates an nValue constraint. Let N be the number of distinct values
  assigned to the variables of the vars collection. Enforce condition
  N = nValues to hold."
  {:choco "nValues(IntVar[] vars, IntVar nValues)"}
  [vars n-values]
  {:pre [(sequential? vars)]}
  (constraint constraint-name
              [(vec vars) n-values]
              compiler))
