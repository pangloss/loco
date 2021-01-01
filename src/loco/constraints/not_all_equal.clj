(ns loco.constraints.not-all-equal
  (:require
   [loco.constraints.utils :refer :all :as utils]
   [loco.constraints.arithm :refer [$arithm]]
   [clojure.spec.alpha :as s]
   [meander.epsilon :as m :refer [match]]
   [loco.utils :refer [p]]
   )
  (:import
   [org.chocosolver.solver.variables IntVar BoolVar SetVar]))

(def ^:private constraint-name 'not-all-equal)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args ::utils/coll-coerce-intvar?))

(compile-function compiler constraint-name [*conformed *model]
 (let [coerce-int-var (p utils/coerce-int-var *model)]
   (match *conformed
     {:args ?vars} (do
                     (.notAllEqual *model
                                   (->> ?vars (map coerce-int-var) (into-array IntVar)))))))

(defn $not-all-equal
  "Constrains that all vars are not equal to each other (different from distinct)"
  {:choco "notAllEqual(IntVar... vars)"}
  ([& more]
   (let [morev (vec more)]
     (match [morev]
       [[?x ?y]]                ($arithm ?x '!= ?y)
       [(m/pred vector? ?vars)] (constraint constraint-name ?vars compiler)))))
