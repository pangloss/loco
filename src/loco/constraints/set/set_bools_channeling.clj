(ns loco.constraints.set.set-bools-channeling

  (:require
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :refer :all :as utils]

   [meander.epsilon :as m :refer [match]]
   [clojure.walk :as walk])
  (:import
   [org.chocosolver.solver.variables SetVar IntVar BoolVar Task]))

(def ^:private constraint-name 'set/bools-channeling)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/tuple set-var?
                               (s/tuple #{'channel} (s/coll-of bool-var?))
                               (s/tuple #{'offset} nat-int?)))))

(compile-function compiler constraint-name [*conformed *model]
 (match *conformed
   {:args [?set-var [_ ?bools] [_ ?offset]]}
   (.setBoolsChanneling *model (into-array BoolVar ?bools) ?set-var ?offset)))

(defn $set-bools-channeling
  "Creates a constraint channeling a set variable with boolean variables :
  i in set <=> bools[i] = TRUE.

  Creates a constraint channeling a set variable with boolean variables :
  i in set <=> bools[i-offset] = TRUE."
  {:choco ["setBoolsChanneling(BoolVar[] bools, SetVar set)"
           "setBoolsChanneling(BoolVar[] bools, SetVar set, int offset)"]}
  ([set-var bools] ($set-bools-channeling set-var bools 0))
  ([set-var bools offset]
   {:pre [(nat-int? offset) (sequential? bools)]}
   (constraint constraint-name
                [set-var
                 ['channel (vec bools)]
                 ['offset  offset]]
                compiler)))
