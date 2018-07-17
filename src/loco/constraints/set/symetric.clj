(ns loco.constraints.set.symetric
  (:use loco.constraints.utils)
  (:require
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :as utils]
   [loco.match :refer [match+]]
   [clojure.core.match :refer [match]]
   [clojure.walk :as walk])
  (:import
   [org.chocosolver.solver.variables SetVar IntVar BoolVar Task]))

(def ^:private constraint-name 'symetric)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/tuple (s/coll-of set-var?)
                               (s/tuple #{'offset} nat-int?)))))

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:args [sets [_ offset]]}
           (.symmetric model (into-array SetVar sets) offset)

           ::s/invalid
           (report-spec-error constraint-name ::compile-spec var-subed-statement))))

(defloco $symetric
  "Creates a constraint stating that sets are symmetric sets: x in sets[y] <=> y in sets[x]
  Creates a constraint stating that sets are symmetric sets: x in sets[y-offset] <=> y in sets[x-offset]"
  {:choco ["symmetric(SetVar... sets)"
           "symmetric(SetVar[] sets, int offset)"]
   :arglists '([sets]
               [sets offset]
               [& set ...])}
  [& sets]
  (match+ (vec sets)
          [set-list offset] :guard [set-list sequential?, offset nat-int?]
          (constraint constraint-name
                      [(vec set-list)
                       ['offset (preserve-consts offset)]]
                      compiler)

          [set-list :guard sequential?] ($symetric set-list 0)
          set-list ($symetric set-list 0)))
