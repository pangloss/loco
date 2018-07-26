(ns loco.constraints.all-different
  (:use loco.constraints.utils)
  (:require
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :as utils]
   [loco.match :refer [match+]]
   [clojure.core.match :refer [match]]
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

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))
        coerce-int-var (partial utils/coerce-int-var model)]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:args [:ints vars]}
           (.allDifferent model (into-array IntVar (map coerce-int-var vars)) "DEFAULT")

           {:args [:sets vars]}
           (.allDifferent model (into-array SetVar vars))

           {:args [:with-consistency {:ints vars :consistency [_ consistency]}]}
           (.allDifferent model (into-array IntVar (map coerce-int-var vars))
                          ({'default "DEFAULT" 'bc "BC" 'ac "AC"} consistency))

           ::s/invalid
           (report-spec-error constraint-name ::compile-spec var-subed-statement))))

(defloco $distinct
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
  (match+ (vec vars)
          [int-vars, {:consistency consistency}]
          ($distinct int-vars consistency)

          [int-vars, consistency]
          :guard [int-vars sequential?, consistency #{:default :bc :ac}]
          (constraint constraint-name
                      [(vec int-vars)
                       ['consistency ({:default 'default :bc 'bc :ac 'ac} consistency)]]
                      compiler)

          [var-list :guard sequential?]
          (constraint constraint-name
                      (vec var-list)
                      compiler)

          [& var-list]
          (constraint constraint-name (vec var-list) compiler)))

(def $all-different $distinct)
(reset-meta! (var $all-different) (meta (var $distinct)))
