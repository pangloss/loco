(ns loco.constraints.scalar
  (:use loco.constraints.utils)
  (:require
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :as utils]
   [loco.match :refer [match+]]
   [clojure.core.match :refer [match]]
   [clojure.walk :as walk])
  (:import
   [org.chocosolver.solver.variables SetVar IntVar BoolVar Task]))

(def ^:private constraint-name 'scalar)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/tuple
                       int-var?
                       comparison-symbol?
                       (s/coll-of int-var?)
                       (s/coll-of int?)))))

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:args [result op vars coeffs]}
           (.scalar model
                    (into-array IntVar vars)
                    (int-array coeffs)
                    (name op)
                    result)

           ::s/invalid
           (report-spec-error constraint-name ::compile-spec var-subed-statement))))

;;TODO: write tests for this related to nested preserve-consts
(declare $scalar)
(defn- constraint-fn
  "handle syntax like ($= :v ($scalar :a :b :c [100 10 1]))"
  [var-name [op [vars coeffs]]]
  ($scalar var-name = vars coeffs))

(defn- name-fn [partial]
  (match partial
         [partial-name [vars coeffs]]
         (->> [(map name vars) (repeat "*") coeffs]
              (apply map vector)
              (interpose "+")
              flatten
              (apply str (name partial-name) "_" ))))

;;shit, don't even know what the body of this is going to look like. it has preserved consts, and vars in parallel arrays, maybe they could be joined
(defn- domain-fn [partial]
  (match partial
         [partial-name [vars coeffs]]
         (->
          (reduce
           (fn [{:keys [lb ub] :as acc} [domain coeff]]
             (match domain
                    ;;TODO: handle enumerated domains
                    {:int true :lb d-lb :ub d-ub} {:lb (min lb d-lb)
                                                   :ub (min ub d-ub)}))
           ;;{:lb 0 :ub 0}
           (zipmap vars coeffs))
          (assoc :int true)
          (update :lb int)
          (update :ub int))))

;; [[:scalar [_ coeffs]] domains]
;; (into [:int] (->> domains lb-ub-seq
;;                   (map #(multiply-domains %2 [%1 %1]) coeffs)
;;                   (reduce add-domains)))

(defn- scalar-partial [body]
  (partial-constraint constraint-name body name-fn constraint-fn domain-fn))

;;TODO: scalar can accept a list of [[int-var coeff] ...] tuples
(defn $scalar
  "Creates a scalar constraint which ensures that Sum(vars[i]*coeffs[i]) operator scalar"
  {:choco "scalar(IntVar[] vars, int[] coeffs, String operator, IntVar scalar)"
   :partial true}
  ([int-vars coeffs]
   {:pre [(sequential? int-vars)
          (every? int? coeffs)]}
   (scalar-partial [(vec int-vars) (preserve-consts coeffs)]))

  ([eq operator int-vars coeffs]
   {:pre [(sequential? int-vars)
          (comparison-operator? operator)
          (every? int? coeffs)]}
   (constraint constraint-name
               [$scalar (to-operator operator) int-vars (preserve-consts coeffs)]
               compiler)))
