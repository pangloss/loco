(in-ns 'loco.constraints)
(ns loco.constraints.min
  (:use loco.constraints.utils)
  (:require
   [loco.utils :refer [p c]]
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :as utils]
   [loco.match :refer [match+]]
   [clojure.core.match :refer [match]]
   [clojure.walk :as walk]
   [defun.core :refer [defun]])
  (:import
   [org.chocosolver.solver.variables SetVar IntVar BoolVar Task]))

(def ^:private constraint-name 'min)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/or
                       :bools       (s/tuple
                                     bool-var?
                                     (s/tuple #{'of}         (s/coll-of bool-var?)))

                       :ints        (s/tuple
                                     int-var?
                                     (s/tuple #{'of}         (s/coll-of int-var?)))

                       :set         (s/tuple
                                     int-var?
                                     (s/tuple #{'of}         set-var?)
                                     (s/tuple #{'not-empty?} boolean?))

                       :set-indices (s/tuple
                                     int-var?
                                     (s/tuple #{'of}         (s/coll-of int?))
                                     (s/tuple #{'indices}    set-var?)
                                     (s/tuple #{'offset}     nat-int?)
                                     (s/tuple #{'not-empty?} boolean?))))))

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:args [(:or :ints :bools) [min [_ vars]]]}
           (.min model min (into-array vars))

           ;;{:args [:bools [min [_ vars]]]}

           {:args [:set [min [_ set] [_ not-empty?]]]}
           (.min model set min not-every?) ;;fugly API! bad choco!

           {:args [:set-indices [min [_ weights] [_ indices] [_ offset] [_ not-empty?]]]}
           (.min model indices (int-array weights) offset min not-every?)

           ::s/invalid
           (report-spec-error constraint-name ::compile-spec var-subed-statement))))

(defn- min-partial [& vars]
  (partial-constraint [:min (vec vars)]))

(defun $min
  "The minimum of several arguments. The arguments can be a mixture of int-vars and numbers
  Creates a constraint over the minimum element in a set: min{i | i in set} = minElementValue
  Creates a constraint over the minimum element induces by a set: min{weights[i-offset] | i in indices} = minElementValue"
  {:choco
   ["min(IntVar min, IntVar[] vars)"
    "min(BoolVar max, BoolVar[] vars)"
    "min(SetVar set, IntVar minElementValue, boolean notEmpty)"
    "min(SetVar indices, int[] weights, int offset, IntVar minElementValue, boolean notEmpty)"]
   :arglists '([min-list]
               [min vars]
               [set-var min not-empty?]
               [set-indices weights offset min not-empty?]
               [& int-vars])}

  ([(min-list :guard sequential?)] (apply min-partial min-list))

  ([min (vars :guard sequential?)]
   (constraint constraint-name
               [min
                ['of (vec vars)]] compiler))

  ([set-var min (not-empty? :guard boolean?)]
   (constraint constraint-name
               [min
                ['of set-var]
                ['not-empty? not-empty?]]
               compiler))

  ([set-indices,
    (weights :guard [sequential? (p every? int?)])
    (offset :guard integer?)
    min,
    (not-empty? :guard boolean?)]
   (constraint constraint-name
               [min
                ['of         (preserve-consts (vec weights))]
                ['indices    set-indices]
                ['offset     (preserve-consts offset)]
                ['not-empty? not-empty?]]
               compiler))

  ([& int-vars] (apply min-partial int-vars)))
