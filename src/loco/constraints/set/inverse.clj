(ns loco.constraints.set.inverse
  (:use loco.constraints.utils)
  (:require
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :as utils]
   [loco.match :refer [match+]]
   [clojure.core.match :refer [match]]
   [clojure.walk :as walk])
  (:import
   [org.chocosolver.solver.variables SetVar IntVar BoolVar Task]))

(def ^:private constraint-name 'inverse)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/tuple
                       (s/tuple #{'inverse-sets} (s/coll-of set-var?) #{'offset} nat-int?)
                       (s/tuple #{'sets} (s/coll-of int-var?) #{'offset} nat-int?)))))

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:args [[_ inverse-sets _ offset-inverse-set] [_ sets _ offset-set]]}
           (.inverseSet model
                        (into-array SetVar sets)
                        (into-array SetVar inverse-sets)
                        offset-set
                        offset-inverse-set)

           ::s/invalid
           (report-spec-error constraint-name ::compile-spec var-subed-statement))))

(defn $inverse
  "Creates a constraint stating that : x in sets[y-offset1] <=> y in invSets[x-offset2]"
  {:choco ["inverseSet(SetVar[] sets, SetVar[] invSets, int offset1, int offset2)"]}
  ([sets offset-set inverse-sets offset-invsere-set]
   {:pre [(integer? offset-invsere-set)
          (integer? offset-set)
          (sequential? inverse-sets)
          (sequential? sets)]}
   (constraint constraint-name
               [['inverse-sets (vec inverse-sets)
                 'offset (preserve-consts offset-invsere-set)]
                ['sets (vec sets)
                 'offset (preserve-consts offset-set)]]
               compiler)))
