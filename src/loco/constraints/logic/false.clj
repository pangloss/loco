(ns loco.constraints.logic.false
  (:require
   [clojure.core.match :refer [match]]
   [clojure.spec.alpha :as s]
   [clojure.walk :as walk]
   [loco.constraints.utils :refer :all :as utils]
   )
  (:import
   [org.chocosolver.solver.variables IntVar BoolVar]))

(def ^:private constraint-name 'false)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args (s/spec
                #{nil})))

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:args [eq _= dividend _ divisor]}
           (.falseConstraint model)

           ::s/invalid
           (report-spec-error constraint-name ::compile-spec var-subed-statement))))

(defloco $false
  "Constraint that is always false"
  {:choco "false()"}
  []
  (constraint constraint-name nil compiler))

;;(defloco $false [] (constraint 'false nil identity))
