(ns loco.constraints.sum
  (:require
   [clojure.core.match :refer [match]]
   [clojure.spec.alpha :as s]
   [clojure.walk :as walk]
   [loco.constraints.all-equal :refer [$=]]
   [loco.constraints.views.offset :refer [$offset]]
   [loco.constraints.utils :refer :all :as utils]
   [loco.utils :refer [p c split]]
   )
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
   (let [[numbers vars] (split int? vars)]
     (match
      [vars numbers]
      [[] []] nil
      [[] nums] ($= summation-var (apply + nums))
      [[only-var] []] ($= summation-var only-var)
      [[only-var] nums] ($= summation-var ($offset only-var (apply + nums)))
      [vars nums] (constraint constraint-name
                              [summation-var (to-operator operator)
                               (conj vars (apply + nums))]
                              compiler)
      )
     )))

;; -------------------- partial --------------------

(def ^:private partial-name '+)

(defn- constraint-fn [& partial]
  (let [[var-name [op body]] partial]
    (let [[numbers vars] (split int? body)
          return (match
                  [vars numbers]
                  [[] []] nil
                  [[] nums] (apply + nums)
                  [[only-var] []] only-var
                  [[only-var] nums] ($offset only-var (apply + nums))
                  [_ _] ($sum var-name '= body)
                  )]
      [return]
      )
    ))

(defn- domain-fn
  ([partial]
   (->
    (match partial
           [partial-name []] {:lb 0 :ub 0}
           [partial-name body]
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
             )))
    (assoc :int true))))

(defloco $+
  "partial of $sum

  e.g.:
  ($= :eq ($+ :n1 :n2 :n3 4)) => ($sum :eq := :n1 :n2 :n3 4)
  "
  {:partial true}
  [& args]
  (partial-constraint
   partial-name
   (vec args)
   :constraint-fn constraint-fn
   :domain-fn domain-fn))
