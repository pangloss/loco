(ns loco.constraints.not-all-equal
  (:require
   [loco.constraints.utils :refer :all]
   [loco.constraints :refer [$arithm]]
   [clojure.spec.alpha :as s]
   [clojure.core.match :refer [match]]
   [clojure.walk :as walk])
  (:import
   [org.chocosolver.solver.variables IntVar BoolVar SetVar]))

(def ^:private constraint-name 'not-all-equal)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args (s/spec
                (s/coll-of int-var?))))

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:args vars}
           (.notAllEqual model (into-array IntVar vars))

           ::s/invalid
           (report-spec-error constraint-name ::compile-spec var-subed-statement))))

(defloco $not-all-equal
  "Constrains that all vars are not equal to each other (different from distinct)"
  {:choco "notAllEqual(IntVar... vars)"}
  [vars]
  {:pre [(sequential? vars)]}
  (constraint constraint-name (vec vars) compiler))

(defloco $not=
  [& more]
  (let [morev (vec more)]
    (match [morev]
           [[x y]] ($arithm x '!= y)
           :else   ($not-all-equal morev))))

(defloco $!= [& more] (apply $not= more))
(reset-meta! (var $!=) (meta (var $not-all-equal)))
(reset-meta! (var $not=) (meta (var $not-all-equal)))
