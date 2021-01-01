(ns loco.constraints.all-different
  (:require
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :refer :all :as utils]
   [meander.epsilon :as m :refer [match]]
   [clojure.walk :as walk])
  (:import
   [org.chocosolver.solver.variables SetVar IntVar BoolVar]))

(def ^:private constraint-name 'all-different)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/or
                       :sets   (s/coll-of set-var?)
                       :ints   ::utils/coll-coerce-intvar?
                       :with-consistency
                       (s/spec
                        (s/cat
                         :ints ::utils/coll-coerce-intvar?
                         :consistency (s/tuple #{'consistency} #{'default 'bc 'ac})))))))

(compile-function compiler constraint-name [*conformed *model]
 (let [coerce-int-var (partial utils/coerce-int-var *model)]
   (match *conformed
     {:args [:ints ?vars]}
     (.allDifferent *model (into-array IntVar (map coerce-int-var ?vars)) "DEFAULT")

     {:args [:sets ?vars]}
     (.allDifferent *model (into-array SetVar ?vars))

     {:args [:with-consistency {:ints ?vars :consistency [_ ?consistency]}]}
     (.allDifferent *model (into-array IntVar (map coerce-int-var ?vars))
                    ({'default "DEFAULT" 'bc "BC" 'ac "AC"} ?consistency)))))

(defn $distinct
  "-------------------- Ints --------------------
  Creates an allDifferent constraint.
  Ensures that all variables from vars take a different value.
  Uses BC plus a probabilistic AC propagator to get a compromise between BC and AC

  BC: Based on: 'A Fast and Simple Algorithm for Bounds Consistency of the AllDifferent Constraint'
  A. Lopez-Ortiz, CG. Quimper, J. Tromp, P.van Beek
  AC: Uses Regin algorithm Runs in O(m.n) worst case time for the initial propagation and then in O(n+m) on average.

  DEFAULT:
  Uses BC plus a probabilistic AC propagator to get a compromise between BC and AC

  -------------------- Sets --------------------
  Creates a constraint stating that sets should all be different (not
  necessarily disjoint) Note that there cannot be more than one empty
  set."
  {:choco ["allDifferent(IntVar... vars)"
           "allDifferent(SetVar... sets)"
           "allDifferent(IntVar[] vars, String CONSISTENCY)"]
   :arglists '([sets-list]
               [& set-vars]
               [ints-list]
               [& int-vars]
               [ints-list {:consistency #{:default :bc :ac}}]
               [ints-list #{:default :bc :ac}])}
  [& vars]
  (let [vars (vec vars)]
    (match vars
      [?int-vars, {:consistency ?consistency}]
      ($distinct (vec (distinct ?int-vars)) ?consistency)

      [(m/pred sequential? ?int-vars) (m/pred #{:default :bc :ac} ?consistency)]
      (constraint constraint-name
                  [(vec (distinct ?int-vars))
                   ['consistency ({:default 'default :bc 'bc :ac 'ac} ?consistency)]]
                  compiler)

      [(m/pred sequential? ?var-list)]
      (constraint constraint-name
                  (vec (distinct ?var-list))
                  compiler)

      [& ?var-list]
      (constraint constraint-name (vec (distinct ?var-list)) compiler))))
