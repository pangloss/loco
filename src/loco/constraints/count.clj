(ns loco.constraints.count
  (:use loco.constraints.utils)
  (:require
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :as utils]
   [loco.match :refer [match+]]
   [clojure.core.match :refer [match]]
   [clojure.walk :as walk])
  (:import
   [org.chocosolver.solver.variables SetVar IntVar BoolVar]))

(def ^:private constraint-name 'count)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/tuple
                       (s/coll-of int-var?)
                       (s/tuple #{'value} int-or-intvar?)
                       (s/tuple #{'limit} int-var?)
                       ))))

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:args [vars [_ value] [_ limit]]}
           (.count model value (into-array IntVar vars) limit)

           ::s/invalid
           (report-spec-error constraint-name ::compile-spec var-subed-statement))))

(defn count
  "Creates a count constraint. Let N be the number of variables of the
  vars collection assigned to value value;
  Enforce condition N = limit to hold. "
  {:choco ["count(int value, IntVar[] vars, IntVar limit) "
           "count(IntVar value, IntVar[] vars, IntVar limit)"]}
  [value vars limit]
  {:pre [(sequential? vars)]}
  (constraint constraint-name
              [(vec vars)
               ['value (preserve-consts value)]
               ['limit limit]]
              compiler))
