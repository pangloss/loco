(in-ns 'loco.constraints)
(ns loco.constraints.abs
  (:use loco.constraints.utils)
  (:require
   [clojure.spec.alpha :as s]
   [loco.utils :refer [p c]]
   [loco.constraints.utils :as utils]
   [clojure.core.match :refer [match]]
   [loco.match :refer [match+]]
   [clojure.walk :as walk]))

(def ^:private constraint-name 'abs)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args (s/spec
                (s/tuple int-var? #{'=} int-var?))))

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:args [eq-var _ operand1]}
           (.absolute model eq-var operand1)

           ::s/invalid
           (report-spec-error constraint-name ::compile-spec var-subed-statement))))

(defn $abs
  "Creates an absolute value constraint:
  eq = |operand|

  eq      = IntVar
  operand = IntVar"
  {:choco "absolute(IntVar var1, IntVar var2)"
   :partial true}
  ([operand]
   (partial-constraint [:abs [operand]]))
  ([eq operand]
   (constraint constraint-name
               [eq '= operand]
               compiler)))
