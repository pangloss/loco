(ns loco.constraints.at-most-n-values
  (:use loco.constraints.utils)
  (:require
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :as utils]
   [loco.match :refer [match+]]
   [clojure.core.match :refer [match]]
   [clojure.walk :as walk])
  (:import
   [org.chocosolver.solver.variables IntVar]))

(def ^:private constraint-name 'at-most-n-values)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/cat
                       :ints     (s/coll-of int-var?)
                       :n-values (s/spec (s/tuple #{'n-values} int-var?))
                       :strong   (s/spec (s/tuple #{'ac} boolean?))))))

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:args {:ints vars :n-values [_ n-values] :strong [_ strong]} }
           (.atMostNValues model
                           (into-array IntVar vars)
                           n-values
                           strong)
           ::s/invalid
           (report-spec-error constraint-name ::compile-spec var-subed-statement))))

(defn at-most-n-values
  "Creates an atMostNValue constraint.
  Let N be the number of distinct values assigned to the variables of the vars collection.
  Enforce condition N <= nValues to hold."
  {:choco "atMostNValues(IntVar[] vars, IntVar nValues, boolean STRONG)"}
  ([vars n-values] (at-most-n-values vars n-values false))
  ([vars n-values strong]
   {:pre [(sequential? vars) (boolean? strong)]}
   (constraint constraint-name
               [(vec vars)
                ['n-values n-values]
                ['strong strong]]
               compiler)))
