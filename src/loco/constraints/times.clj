(ns loco.constraints.times
  (:require
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :refer [constraint partial-constraint with-compiler] :as utils]
   [clojure.core.match :refer [match]]
   [clojure.walk :as walk])
  (:import
   [org.chocosolver.solver.variables IntVar BoolVar]))

(def ^:private constraint-name :times)

(s/def ::compile-spec
  (s/cat :constraint #{'times}
         :args (s/spec
                (s/tuple utils/int-var? #{'=} utils/int-var? #{'*} utils/int-var?))))

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:constraint 'times ,:args [eq-var _ operand1 _ operand2]}
           (.times model operand1 operand2 eq-var)

           ::s/invalid
           (utils/report-spec-error constraint-name ::compile-spec var-subed-statement))))

(defn times
  "Creates a multiplication constraint: X * Y = Z, they can all be
  IntVars. seems similar to arithm... you should probably use arithm
  instead, for readability"
  {:choco "times(IntVar X, IntVar Y, IntVar Z)"}
  ([eq = operand1 * operand2]
   (-> (constraint ['times [eq '= operand1 '* operand2]])
       (with-compiler compiler)))
  ([eq operand1 operand2]
   (-> (constraint ['times [eq '= operand1 '* operand2]])
       (with-compiler compiler))))
