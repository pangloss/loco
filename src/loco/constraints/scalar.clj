;; FIXME: WIP

(ns loco.constraints.scalar
  (:use loco.constraints.utils)
  (:require
   [clojure.core.match :refer [match]]
   [clojure.spec.alpha :as s]
   [clojure.walk :as walk]
   [loco.constraints.utils :refer :all :as utils]
   [loco.match :refer [match+]]
   )
  (:import
   [org.chocosolver.solver.variables
    SetVar
    IntVar
    BoolVar
    Task]))

(def ^:private constraint-name 'scalar)

;;FIXME: this is very broken, the format is [var '= [[1 a] [10 b] [100 c]]]
(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/tuple
                       int-var?
                       comparison-symbol?
                       (s/coll-of int-var?)
                       (s/coll-of int?)))))

;;FIXME: this is very broken, the format is [var '= [[1 a] [10 b] [100 c]]]
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
  ($scalar var-name '= vars coeffs))

;;FIXME: this is very broken, the format is [var '= [[1 a] [10 b] [100 c]]]
(defn- name-fn [partial]
  (match partial
         [partial-name [vars coeffs]]
         (->> [(map name vars) (repeat "*") coeffs]
              (apply map vector)
              (interpose "+")
              flatten
              (apply str (name partial-name) "_" ))))

;;FIXME: this is very broken, the format is [var '= [[1 a] [10 b] [100 c]]]
;;...shit. don't even know what the body of this is going to look like. it has preserved consts, and vars in parallel arrays, maybe they could be joined
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
(defloco $scalar
  "Creates a scalar constraint which ensures that Sum(vars[i]*coeffs[i]) operator scalar"
  {:choco "scalar(IntVar[] vars, int[] coeffs, String operator, IntVar scalar)"
   :partial true}
  ([int-vars coeffs]
   {:pre [(sequential? int-vars)
          (every? int? coeffs)]}
   (scalar-partial (mapv vector coeffs (repeat '*) int-vars)))

  ([eq operator int-var-coeff-pairs]
   {:pre [(sequential? int-var-coeff-pairs)
          ;; test that every coeff is an int
          ;; test that we are dealing with pairs
          (every? #(and
                    (int? (first %))
                    (= 2 (count %))) int-var-coeff-pairs)]}
   (constraint constraint-name
               [eq '=  (vec int-var-coeff-pairs)]
               compiler))

  ([eq operator int-vars coeffs]
   {:pre [(sequential? int-vars)
          (every? int? coeffs)
          (= (count int-vars) (count coeffs))]}
   (constraint constraint-name
               [eq '=  (mapv vector coeffs int-vars)]
               compiler)))
