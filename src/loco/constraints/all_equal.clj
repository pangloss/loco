(ns loco.constraints.all-equal
  (:require
   [meander.epsilon :as m :refer [match]]
   [clojure.spec.alpha :as s]
   [clojure.walk :as walk]
   [loco.constraints.utils :refer :all :as utils]
   [loco.utils :refer [p]]
   [loco.constraints.arithm :use [$arithm]]
   )
  (:import
   [org.chocosolver.solver.variables
    IntVar
    BoolVar
    SetVar]))

(def ^:private constraint-name '=)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args (s/spec
                (s/or
                 :ints ::utils/coll-coerce-intvar?
                 :sets ::utils/coll-setvar?))))

(compile-function compiler constraint-name [*conformed *model]
 (let [coerce-int-var (p utils/coerce-int-var *model)]
   (match *conformed
     {:args [:ints ?vars]}
     (.allEqual *model (->> ?vars (map coerce-int-var) (into-array IntVar)))

     {:args [:sets ?vars]}
     (.allEqual *model (into-array SetVar ?vars)))))

(defn $=
  "Constrains that all vars are equal to each other

  Creates a constraint stating that ints should be all equal.
  Creates a constraint stating that sets should be all equal."
  {:choco ["allEqual(IntVar... vars)"
           "allEqual(SetVar... sets)"]}
  [& more]
  (match (vec more)
    [(m/pred vector? ?single-arg-as-vec)]
    (constraint constraint-name (vec ?single-arg-as-vec)
                compiler)
    [?a ?b] ($arithm ?a '= ?b)
    _ (constraint constraint-name (vec more) compiler)))
