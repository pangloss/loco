(ns loco.constraints.cardinality
  (:require
   [loco.constraints.vars :refer [$int]]
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :refer :all :as utils]
   [meander.epsilon :as m :refer [match]]
   [clojure.walk :as walk]
   [loco.utils :refer [p]]
   )
  (:import
   [org.chocosolver.solver.variables IntVar BoolVar]))

(def ^:private constraint-name 'cardinality)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/tuple
                       ::utils/coll-coerce-intvar?
                       map?
                       (s/tuple #{'closed} boolean?)
                       ))))

(defn- to-intvar [model var]
  (cond
    (int? var) (.intVar model var)
    (instance? IntVar var) var))

(compile-function
 (let [coerce-var (p utils/coerce-var *model)]
   (match *conformed
     {:args [?vars ?cardinality-map [_ ?closed?]]}
     (.globalCardinality
      *model
      (->> ?vars (map coerce-var) (into-array IntVar))
      (->> ?cardinality-map keys int-array)
      (->> ?cardinality-map vals (map (p to-intvar *model)) (into-array IntVar))
      ?closed?))))

;;TODO: i forget if cardinality is a partial or has some sort of properties of a partial, but it had a domain function in the old model.clj code.
;; [var-name [:constraint ['cardinality [vars [values occurences] _]]] dep-domains]
;; (cardinality-domain var-name values occurences dep-domains)

(defn $cardinality
  "Takes a list of variables, and a frequency map (from numbers to
  frequencies), constrains that the frequency map is accurate. If
  the :closed flag is set to true, any keys that aren't in the
  frequency map can't appear at all in the list of variables.

  cardinality will generate vars in the model/compile phase from the frequencies values

  Example: ($cardinality [:a :b :c :d :e] {1 :ones, 2 :twos})
  => {:a 1, :b 1, :c 2, :d 2, :e 2 :ones 2, :twos 3}

Creates a global cardinality constraint (GCC): Each value values[i]
should be taken by exactly occurrences[i] variables of vars.

This constraint does not ensure any well-defined level of consistency, yet.

Parameters:
    vars - collection of variables
    values - collection of constrained values
    occurrences - collection of cardinality variables
    closed - restricts domains of vars to values if set to true
"
  {:choco "globalCardinality(IntVar[] vars, int[] values, IntVar[] occurrences, boolean closed)"
   :gccat "http://sofdem.github.io/gccat/gccat/Cglobal_cardinality.html"}
  ([variables frequencies]
   ($cardinality variables frequencies false))

  ([variables frequencies closed?]
   {:pre [
          (map? frequencies)
          (sequential? variables)
          (some? (#{:closed true false} closed?))
          (every? int? (keys frequencies))
          (distinct? (keys frequencies))
          ]
    }
   (let [closed (get {:closed true} closed? closed?)
         values (vec (keys frequencies))
         occurences (vec (vals frequencies))
         generated-vars (->> occurences
                             distinct
                             (remove int?)
                             (mapv #($int % (vec (range (count variables))))))
         cardinality-constraint (constraint constraint-name
                                            [(vec variables)
                                             frequencies ;;[values occurences]
                                             ['closed closed]]
                                            compiler)]
     (->
      (vec (concat generated-vars [cardinality-constraint]))
      (with-meta {:generated-vars true})))))
