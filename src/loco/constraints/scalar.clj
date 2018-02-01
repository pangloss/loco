(in-ns 'loco.constraints)
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

;;TODO: scalar can accept a list of [[int-var coeff] ...] tuples
(defn $scalar
  "Creates a scalar constraint which ensures that Sum(vars[i]*coeffs[i]) operator scalar"
  {:choco "scalar(IntVar[] vars, int[] coeffs, String operator, IntVar scalar)"
   :partial true}
  ([int-vars coeffs]
   {:pre [(sequential? int-vars)
          (every? int? coeffs)]}
   (partial-constraint
    [:scalar [(vec int-vars) (preserve-consts coeffs)]]))

  ([eq operator int-vars coeffs]
   {:pre [(sequential? int-vars)
          (comparison-operator? operator)
          (every? int? coeffs)]}
   (constraint constraint-name
               [scalar (to-operator operator) int-vars (preserve-consts coeffs)]
               compiler)))
