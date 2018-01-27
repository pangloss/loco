(ns loco.constraints.div
  (:use loco.constraints.utils)
  (:require
   [clojure.spec.alpha :as s]
   [loco.utils :refer [p c]]
   [loco.constraints.utils :as utils]
   [clojure.core.match :refer [match]]
   [loco.match :refer [match+]]
   [clojure.walk :as walk]))

(def ^:private constraint-name 'div)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args (s/spec
                (s/tuple int-var? #{'=} int-var? #{'/} int-var?))))

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:args [eq _= dividend _ divisor]}
           (.div model dividend divisor eq)

           ::s/invalid
           (report-spec-error constraint-name ::compile-spec var-subed-statement))))

(defn div
  "Creates an euclidean division constraint.

  Ensures eq = dividend / divisor
  rounding towards 0 Also ensures divisor != 0

  eq       = IntVar
  dividend = IntVar
  divisor  = IntVar"
  {:choco "div(IntVar dividend, IntVar divisor, IntVar result)"
   :partial true}
  ([dividend, divisor]
   (partial-constraint [:/ [dividend, divisor]]))

  ([eq = dividend / divisor] (div eq dividend divisor))

  ([eq dividend, divisor]
   (constraint constraint-name
               [eq '= dividend '/ divisor]
               compiler)))
