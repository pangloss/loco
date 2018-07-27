(ns loco.constraints.bools-int-channeling
  (:use loco.constraints.utils)
  (:require
   [clojure.core.match :refer [match]]
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

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))
        coerce-var (p utils/coerce-var model)]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:args [[_ bools] [_ int-var] [_ offset]]}
           (.boolsIntChanneling model
                                (->> bools (map coerce-var) (into-array BoolVar))
                                (coerce-var int-var)
                                offset)

           ::s/invalid
           (report-spec-error constraint-name ::compile-spec var-subed-statement))))

(defloco $bools-int-channeling
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
