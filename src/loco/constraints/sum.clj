(ns loco.constraints.sum
  (:use loco.constraints.utils)
  (:require
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :as utils]
   [clojure.core.match :refer [match]]
   [clojure.walk :as walk])
  (:import
   [org.chocosolver.solver.variables IntVar BoolVar]))

(def ^:private constraint-name 'sum)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args (s/spec
                (s/or
                 :set   (s/cat :eq-var int-var?
                               :op #{'=}
                               :var set-var?)
                 :bools (s/cat :eq-var int-var?
                               :op comparison-operator?
                               :vars (s/spec ::utils/bool-vars))
                 :ints  (s/cat :eq-var int-var?
                               :op comparison-operator?
                               :vars (s/spec ::utils/int-vars))))))

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:args [:ints {:eq-var eq-var :op op, :vars vars}]}
           (.sum model (into-array IntVar vars) (name op) eq-var)

           {:args [:bools {:eq-var eq-var :op op, :vars vars}]}
           (.sum model (into-array BoolVar vars) (name op) eq-var)

           {:args [:set {:eq-var eq-var :op '=, :var set-var}]}
           (.sum model set-var eq-var)

           ::s/invalid
           (utils/report-spec-error constraint-name ::compile-spec var-subed-statement))))

(defn sum
  "Creates a sum constraint.

  constrain a var to be compared to the sum of a set or list of integers or booleans

  summation = IntVar
  set-var   = SetVar
  vars      = IntVar[] | BoolVar[]

  operator  of #{'= '> '< '!= '>= '<=}"
  {:choco ["sum(BoolVar[] vars, String operator, IntVar sum)"
           "sum(IntVar[]  vars, String operator, IntVar sum)"
           "sum(SetVar set, IntVar sum)"]}
  ([summation set-var]
   (constraint constraint-name
               [summation '= set-var]
               compiler))

  ([summation operator vars]
   {:pre [(sequential? vars)
          (comparison-operator? operator)]}
   (constraint constraint-name
               [summation (to-operator operator) vars]
               compiler)))

(defn +
  "partial of $sum

  e.g.:
  ($= :eq ($+ :n1 :n2 :n3 4))
  "
  {:partial true}
  ([& args]
   (partial-constraint [:+ (vec args)])))
