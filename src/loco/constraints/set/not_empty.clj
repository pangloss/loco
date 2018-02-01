(in-ns 'loco.constraints)
(ns loco.constraints.set.not-empty
  (:use loco.constraints.utils)
  (:require
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :as utils]
   [loco.match :refer [match+]]
   [clojure.core.match :refer [match]]
   [clojure.walk :as walk])
  (:import
   [org.chocosolver.solver.variables SetVar IntVar BoolVar Task]))

(def ^:private constraint-name 'not-empty)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       set-var?))

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:args set-var}
           (.notEmpty model set-var)

           ::s/invalid
           (report-spec-error constraint-name ::compile-spec var-subed-statement))))

(defn $not-empty
  "Creates a constraint preventing set to be empty"
  {:choco "notEmpty(SetVar set)"}
  [set-var]
  (constraint constraint-name set-var compiler))
