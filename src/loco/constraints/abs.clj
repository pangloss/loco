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

;; -------------------- partial --------------------

(def ^:private partial-name 'abs)

(defn- name-fn [partial]
  (match partial
         [partial-name [operand]]
         (str "|" (name operand) "|")))

(declare $abs)
(defn- constraint-fn [var-name [op [operand]]]
  ($abs var-name operand))

(defn- domain-fn [[partial-name [{:keys [lb ub]}]]]
  (let [lb (int lb)
        ub (int ub)]
    (-> (match (vec (sort [lb ub])) ;;why don't i trust lb and ub here?
               [(low :guard neg?) (high :guard neg?)] [(Math/abs high) (Math/abs low)]
               [(low :guard neg?) high] [0 (max (Math/abs high) (Math/abs low))]
               [low high] [(Math/abs low) (Math/abs high)]))
    (zipmap [:lb :ub])
    (assoc :int true)))

(defn $abs
  "Creates an absolute value constraint:
  ($abs eq operand) or ($abs eq = operand)
  eq = |operand|

  eq      = IntVar
  operand = IntVar"
  {:choco "absolute(IntVar var1, IntVar var2)"
   :partial true}
  ([operand]
   (partial-constraint partial-name [operand] name-fn constraint-fn domain-fn))
  ([eq operand]
   (constraint constraint-name
               [eq '= operand]
               compiler))
  ([eq _op operand]
   ($abs eq operand)))
