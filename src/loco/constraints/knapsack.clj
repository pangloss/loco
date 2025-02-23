(ns loco.constraints.knapsack
  (:require
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :refer :all :as utils]
   [meander.epsilon :as m :refer [match]]
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

(compile-function compiler constraint-name [*conformed *model]
 (match *conformed
   {:args [[_ ?weights] [_ ?energies] [_ ?occurrences] [_ ?weight-sum] [_ ?energy-sum]]}
   (.knapsack *model
              (into-array IntVar ?occurrences)
              ?weight-sum
              ?energy-sum
              (int-array ?weights)
              (int-array ?energies))))

;;TODO: can we infer domains for the occurances variables?
;;TODO: find knapsack on GCCAT
(defn $knapsack
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
              [['weight  (vec weight)]
               ['energy  (vec energy)]
               ['occurrences (vec occurrences)]
               ['weight-sum weight-sum]
               ['energy-sum energy-sum]]
              compiler))
