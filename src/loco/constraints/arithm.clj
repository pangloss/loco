(ns loco.constraints.arithm
  (:require
   [clojure.spec.alpha :as s]
   [loco.utils :refer [p c]]
   [loco.constraints.utils :refer
    [
     preserve-consts,
     constraint partial-constraint
     with-compiler]
    :as utils]
   [clojure.core.match :refer [match]]
   [loco.match :refer [match+]]
   [clojure.walk :as walk])
  (:import
   [org.chocosolver.solver.variables BoolVar IntVar]))

(def ^:private constraint-name :arithm)

(s/def ::compile-spec
  (s/cat :constraint #{:con/arithm}
         :args (s/or
                :compare (s/cat :eq-var utils/int-var?
                                :compare-op utils/qualified-comparison-operator?
                                :operand utils/int-or-intvar?)
                :arithm (s/cat :eq-var utils/int-var?
                               :compare-op utils/qualified-comparison-operator?
                               :operand1 utils/int-var?
                               :arithm-op utils/qualified-arithmetic-operator?
                               :operand2 utils/int-or-intvar?))))

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:constraint :con/arithm,:args
            [:compare {:eq-var eq-var :compare-op op, :operand var}]}
           (.arithm model eq-var (name op) var)

           {:constraint :con/arithm, :args
            [:arithm {:eq-var eq-var :compare-op op, :operand1 var
                       :arithm-op op2 :operand2 var2}]}
           (.arithm model eq-var (name op) var (name op2) var2)

           ::s/invalid
           (utils/report-spec-error constraint-name ::compile-spec var-subed-statement))))

(defn arithm
  "similar to choco arithm. lets you use division with an IntVar. other
  than that it is a shortcut for having a compare and operation in 1
  instruction. lets you write a = b + c. allowed operators are
  #{:+ :* :/ :-}, allowed comparisons are #{:= :> :< :!= :>= :<=}
  a, b and c are allowed to be partial constraints"
  {:choco ["arithm(IntVar var,  String op,  int cste)"
           "arithm(IntVar var1, String op,  IntVar var2)"
           "arithm(IntVar var1, String op1, IntVar var2, String op2, int cste)"
           "arithm(IntVar var1, String op1, IntVar var2, String op2, IntVar var3)"]}
  ([a compare b]
   {:pre [(utils/comparison-operator? compare)]}
   (let [compare (compare utils/qualified-operator-map)]
     (-> (constraint [:con/arithm [a compare (preserve-consts b)]])
         (with-compiler compiler))))

  ([a compare b op c]
   {:pre [(utils/comparison-operator? compare)
          (utils/arithmetic-operator? op)]}
   (let [op (op utils/qualified-arithmetic-operator-map)
         compare (compare utils/qualified-operator-map)]
     (-> (constraint [:con/arithm [a compare b op (preserve-consts c)]])
         (with-compiler compiler)))))
