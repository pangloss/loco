(ns loco.constraints.count
  (:require
   [meander.epsilon :as m :refer [match]]
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
  (compile-function
   (let [coerce-var (utils/coerce-var *model)]
     (match *conformed
       {:args [?vars [_ ?value] [_ ?limit]]}
       (.count *model
               (coerce-var ?value)
               (->> ?vars (map coerce-var) (into-array IntVar))
               (coerce-var ?limit))))))

;;TODO: possible partial ($= :value ($count [1 2 3] :limit))
;; it's also possible to do ($= :limit ($count :value [1 2 3]))
;; so it's hard to know which one is the common case.
(defn $count
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
