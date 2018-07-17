(ns loco.constraints.knapsack
  (:use loco.constraints.utils)
  (:require
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :as utils]
   [loco.match :refer [match+]]
   [clojure.core.match :refer [match]]
   [clojure.walk :as walk])
  (:import
   [org.chocosolver.solver.variables SetVar IntVar BoolVar Task]))

(def ^:private constraint-name 'knapsack)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/tuple
                       (s/tuple #{'weight} (s/coll-of nat-int?))
                       (s/tuple #{'energy} (s/coll-of nat-int?))
                       (s/tuple #{'occurrences} (s/coll-of int-var?))
                       (s/tuple #{'weight-sum} int-var?)
                       (s/tuple #{'energy-sum} int-var?)))))

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:args [[_ weights] [_ energies] [_ occurrences] [_ weight-sum] [_ energy-sum]]}
           (.knapsack model
                      (into-array IntVar occurrences)
                      weight-sum
                      energy-sum
                      (int-array weights)
                      (int-array energies))

           ::s/invalid
           (report-spec-error constraint-name ::compile-spec var-subed-statement))))

;;TODO: find knapsack on GCCAT
(defloco $knapsack
  "Creates a knapsack constraint. Ensures that :
  - occurrences[i] * weight[i] = weightSum
  - occurrences[i] * energy[i] = energySum
  and maximizing the value of energySum.

  Example: ($knapsack [3 1 2]    ; weight of item
                      [5 6 7]    ; energy of item
                      [:x :y :z] ; occurrences
                      :W         ; total weight
                      :V)        ; total energy"
  {:choco "knapsack(IntVar[] occurrences, IntVar weightSum, IntVar energySum, int[] weight, int[] energy)"}
  [weight energy occurrences weight-sum energy-sum]
  {:pre [
         (every? nat-int? weight)
         (every? nat-int? energy)
         (sequential? occurrences)
         ]}
  (constraint constraint-name
              [['weight (preserve-consts (vec weight))]
               ['energy (preserve-consts (vec energy))]
               ['occurrences (vec occurrences)]
               ['weight-sum weight-sum]
               ['energy-sum energy-sum]]
              compiler))
