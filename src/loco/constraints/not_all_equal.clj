(ns loco.constraints.not-all-equal
  (:require
   [loco.constraints.utils :refer :all :as utils]
   [loco.constraints.arithm :refer [$arithm]]
   [clojure.spec.alpha :as s]
   [meander.epsilon :as m :refer [match]]
   [clojure.walk :as walk])
  (:import
   [org.chocosolver.solver.variables IntVar BoolVar SetVar]))

(def ^:private constraint-name 'not-all-equal)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args (s/spec
                (s/coll-of int-or-intvar?))))

(compile-function
 (let [coerce-int-var (partial utils/coerce-int-var *model)]
   (match *conformed
     {:args ?vars} (.notAllEqual *model (->> ?vars (map coerce-int-var) (into-array IntVar))))))

(defn $not-all-equal
  "Constrains that all vars are not equal to each other (different from distinct)"
  {:choco "notAllEqual(IntVar... vars)"}
  ([& more]
   (let [morev (vec more)]
     (match [morev]
       [[?x ?y]]                ($arithm ?x '!= ?y)
       [(m/pred vector? ?vars)] (constraint constraint-name ?vars compiler)))))
