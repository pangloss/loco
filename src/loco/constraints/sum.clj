(ns loco.constraints.sum
  (:require
   [loco.utils :refer [p c]]
   [loco.constraints.utils :refer :all :as utils]
   [clojure.spec.alpha :as s]
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
                 :bools (s/cat :eq-var ::utils/int-or-intvar?
                               :op ::utils/comparison-operator?
                               :vars (s/spec ::utils/bool-vars))
                 :ints  (s/cat :eq-var ::utils/int-or-intvar?
                               :op ::utils/comparison-operator?
                               :vars (s/spec (s/coll-of ::utils/coerce-intvar?)))))))

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:args [:ints {:eq-var eq-var :op op, :vars vars}]}
           (.sum model (into-array IntVar (map (p coerce-int-var model) vars)) (name op) eq-var)

           {:args [:bools {:eq-var eq-var :op op, :vars vars}]}
           (.sum model (into-array BoolVar vars) (name op) eq-var)

           {:args [:set {:eq-var eq-var :op '=, :var set-var}]}
           (.sum model set-var eq-var)

           ::s/invalid
           (report-spec-error constraint-name ::compile-spec var-subed-statement))))

(defloco $sum
  "Creates a sum constraint.

  constrain a var to be compared to the sum of a set or list of integers or booleans

  summation = IntVar
  set-var   = SetVar
  vars      = IntVar[] | BoolVar[]

  operator  of #{'= '> '< '!= '>= '<=}"
  {:choco ["sum(BoolVar[] vars, String operator, IntVar sum)"
           "sum(IntVar[]  vars, String operator, IntVar sum)"
           "sum(SetVar set, IntVar sum)"]}
  ([summation-var set-var]
   (constraint constraint-name
               [summation-var '= set-var]
               compiler))

  ([summation-var operator vars]
   {:pre [(sequential? vars)
          (comparison-operator? operator)]}
   (constraint constraint-name
               [summation-var (to-operator operator) vars]
               compiler)))

;; -------------------- partial --------------------

(def ^:private partial-name '+)

(defn- name-fn [partial]
  (match partial
         [partial-name body]
         (apply str (interpose (name partial-name) body))))

(defn- constraint-fn [var-name [op body]]
  [($sum var-name '= body)])

(defn- domain-fn [partial]
  (match partial
         [partial-name body]
         (->
          (->>
           body
           (map domainize) ;;FIXME: leaky abstration
           (reduce
            (fn [{:keys [lb ub] :as acc} domain]
              (match
               domain
               ;;TODO: handle enumerated domains
               {:int true :lb d-lb :ub d-ub} {:lb (unchecked-add (int lb) (int d-lb))
                                              :ub (unchecked-add (int ub) (int d-ub))}))
            ))
          (assoc :int true))))

(defloco $+
  "partial of $sum

  e.g.:
  ($= :eq ($+ :n1 :n2 :n3 4)) => ($sum :eq := :n1 :n2 :n3 4)
  "
  {:partial true}
  ([& args]
   (partial-constraint
    partial-name
    (vec args)
    name-fn
    constraint-fn
    domain-fn)))
