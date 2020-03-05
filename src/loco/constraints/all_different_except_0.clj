(ns loco.constraints.all-different-except-0

  (:require
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :refer :all :as utils]

   [meander.epsilon :as m :refer [match]]
   [clojure.walk :as walk])
  (:import
   [org.chocosolver.solver.variables IntVar]))

(def ^:private constraint-name 'all-different-except-0)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :ints       ::utils/coll-coerce-intvar?))

(compile-function
 (let [coerce-int-var (partial utils/coerce-int-var *model)]
   (match *conformed
     {:ints ?vars}
     (.allDifferentExcept0 *model (->> ?vars (map coerce-int-var) (into-array IntVar))))))

(defn $distinct-except-0
  "Creates an allDifferent constraint for variables that are not equal to 0.
  There can be multiple variables equal to 0."
  {:choco "allDifferentExcept0(IntVar[] vars)"
   :arglists '([ints-list]
               [& int-vars])}
  [& vars]
  {:pre [(sequential? vars)]}
  (match (vec vars)
    [(m/pred sequential? ?var-list)] (constraint constraint-name
                                                 (vec ?var-list)
                                                 compiler)

    [& ?var-list] (constraint constraint-name
                              (vec ?var-list)
                              compiler)))

(def $all-different-except-0 $distinct-except-0)
(alter-meta! (var $all-different-except-0) merge (dissoc (meta (var $distinct-except-0)) :name))
