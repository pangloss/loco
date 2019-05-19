(ns loco.constraints.abs
  (:use loco.constraints.utils)
  (:require
   [loco.constraints :refer :all]
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

;; -------------------- partial --------------------

(def ^:private partial-name 'abs)

(defn- name-fn [partial]
  (match partial
         [partial-name [operand]]
         (str "|" (name operand) "|")))

(declare $abs)

(defn- constraint-fn [var-name [op [operand]]]
  [($abs-view operand)])

(defn- domain-fn [[partial-name [{:keys [lb ub]}]]]
  (let [lb (int lb)
        ub (int ub)
        return (-> (->> [(Math/abs lb) (Math/abs ub)] sort (zipmap [:lb :ub]))
                   (assoc :int true))]
    return))

(defloco $abs
  "Creates an absolute value constraint:
  ($abs eq operand) or ($abs eq = operand)
  eq = |operand|

  eq      = IntVar
  operand = IntVar"
  {:choco "absolute(IntVar var1, IntVar var2)"
   :partial true}
  ([operand]
   (partial-constraint partial-name [operand]
                       :name-fn name-fn
                       :constraint-fn constraint-fn
                       :domain-fn domain-fn))
  ([eq operand]
   (constraint constraint-name
               [eq '= operand]
               compiler))
  ([eq _op operand]
   ($abs eq operand)))
