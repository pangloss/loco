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

;; -------------------- partial --------------------

(def ^:private partial-name '/)

(declare $div)
(defn- constraint-fn [& partial]
  (println 'constraint-fn partial)
  (let [[var-name [op [dividend, divisor]]] partial]
    ($div var-name = dividend / divisor)))

;;TODO: possible that there is a way to optimize this, but i was a bit lazy
(defn- domain-fn [partial]
  (println 'domain-fn partial)
  (let [[_ domains] partial
        [{lb1 :lb ub1 :ub} {lb2 :lb ub2 :ub}] (map domainize domains)
        lb1 (int lb1)
        lb2 (int lb2)
        ub1 (int ub1)
        ub2 (int ub2)
        return (->
                (->>
                 (for  [n [lb1 ub1]
                        d [lb2 ub2]
                        :when (not (zero? d))]
                   ;;prevent div by zero by replacing zeros with 1s
                   ;;TODO: have to handle div/0 better
                   ;; if lb ub [-1 0] then we need to test -1
                   ;; if lb ub [0 2] then we need to test 1
                   ;; if lb ub [-1 5] then we need to test -1 and 1 as divisors (i think)
                   (unchecked-divide-int n d))
                 sort
                 ((juxt first last))
                 (interleave [:lb :ub])
                 (apply hash-map))
                (assoc ,, :int true)
                )] ;; what have i done? what madness!
    return))

;;TODO: create $div with no rounding, as rounding is sometimes undesirable
(defloco $div
  "Creates an euclidean division constraint.

  Ensures eq = dividend / divisor
  rounding towards 0 Also ensures divisor != 0

  eq       = IntVar
  dividend = IntVar
  divisor  = IntVar"
  {:choco "div(IntVar dividend, IntVar divisor, IntVar result)"
   :partial true}
  ([whatever] nil)
  ([dividend, divisor]
   {:pre [(if (int? divisor)
            (not (zero? divisor))
            true)]}
   (partial-constraint
    partial-name [dividend, divisor]
    :constraint-fn constraint-fn
    :domain-fn domain-fn))

  ([eq = dividend / divisor] ($div eq dividend divisor))

  ([eq dividend, divisor]
   {:pre [(if (int? divisor)
            (not (zero? divisor))
            true)]}
   (constraint constraint-name
               [eq '= dividend '/ divisor]
               compiler)))
