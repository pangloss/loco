(ns loco.constraints.bools-int-channeling

  (:require
   [meander.epsilon :as m :refer [match]]
   [clojure.spec.alpha :as s]
   [clojure.walk :as walk]
   [loco.constraints.utils :refer :all :as utils]
   [loco.utils :refer [p]]
   )
  (:import
   [org.chocosolver.solver.variables IntVar BoolVar]))

(def ^:private constraint-name 'bools-int-channeling)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/tuple
                       (s/tuple #{'bool-vars} ::utils/coll-coerce-boolvar?)
                       (s/tuple #{'int-var}   ::utils/coerce-intvar?)
                       (s/tuple #{'offset} int?)
                       ))))

(compile-function compiler constraint-name [*conformed *model]
 (let [coerce-var (p utils/coerce-var *model)]
   (match *conformed
     {:args [[_ ?bools] [_ ?int-var] [_ ?offset]]}
     (.boolsIntChanneling *model
                          (->> ?bools (map coerce-var) (into-array BoolVar))
                          (coerce-var ?int-var)
                          ?offset))))

(defn $bools-int-channeling
  "Creates an channeling constraint between an integer variable and a
  set of boolean variables.
  Maps the boolean assignments variables bVars with the standard assignment variable var.
  var = i <-> bVars[i-offset] = 1"
  {:choco "boolsIntChanneling(BoolVar[] bVars, IntVar var, int offset)"}
  ([bool-vars, int-var] ($bools-int-channeling bool-vars int-var 0))
  ([bool-vars, int-var, offset]
   {:pre [(int? offset) (sequential? bool-vars)]}
   (constraint constraint-name
               [['bool-vars (vec bool-vars)]
                ['int-var int-var]
                ['offset offset]]
               compiler)))
