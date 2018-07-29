(ns loco.constraints.count
  (:require
   [clojure.core.match :refer [match]]
   [clojure.spec.alpha :as s]
   [clojure.walk :as walk]
   [loco.constraints.utils :refer :all :as utils]
   )
  (:import
   [org.chocosolver.solver.variables SetVar IntVar BoolVar]))

(def ^:private constraint-name 'count)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/tuple
                       ::utils/coll-coerce-intvar?
                       (s/tuple #{'value} ::utils/coerce-intvar?)
                       (s/tuple #{'limit} ::utils/coerce-intvar?)
                       ))))

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))
        coerce-var (utils/coerce-var model)]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:args [vars [_ value] [_ limit]]}
           (.count model
                   (coerce-var value)
                   (->> vars (map coerce-var) (into-array IntVar))
                   (coerce-var limit))

           ::s/invalid
           (report-spec-error constraint-name ::compile-spec var-subed-statement))))

;;TODO: possible partial ($= :value ($count [1 2 3] :limit))
(defloco $count
  "Creates a count constraint. Let N be the number of variables of the
  vars collection assigned to value value;
  Enforce condition N = limit to hold. "
  {:choco ["count(int    value, IntVar[] vars, IntVar limit) "
           "count(IntVar value, IntVar[] vars, IntVar limit)"]}
  [value vars limit]
  {:pre [(sequential? vars)]}
  (constraint constraint-name
              [(vec vars)
               ['value  value]
               ['limit limit]]
              compiler))
