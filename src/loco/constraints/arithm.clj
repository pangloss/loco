(ns loco.constraints.arithm
  (:use loco.constraints.utils)
  (:require
   [clojure.spec.alpha :as s]
   [loco.utils :refer [p c]]
   [loco.constraints.utils :as utils]
   [clojure.core.match :refer [match]]
   [loco.match :refer [match+]]
   [clojure.walk :as walk])
  (:import
   [org.chocosolver.solver.variables BoolVar IntVar]))

(def ^:private constraint-name :arithm)

(s/def ::compile-spec
  (s/cat :constraint #{'arithm}
         :args (s/or
                :compare (s/cat :eq-var int-var?
                                :compare-op comparison-symbol?
                                :operand int-or-intvar?)
                :arithm (s/cat :eq-var int-var?
                               :compare-op comparison-symbol?
                               :operand1 int-var?
                               :arithm-op arithmetic-symbol?
                               :operand2 int-or-intvar?))))

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:args [:compare {:eq-var eq-var :compare-op op, :operand var}]}
           (.arithm model eq-var (name op) var)

           {:args [:arithm {:eq-var eq-var :compare-op op, :operand1 var
                            :arithm-op op2 :operand2 var2}]}
           (.arithm model eq-var (name op) var (name op2) var2)

           ::s/invalid
           (utils/report-spec-error constraint-name ::compile-spec var-subed-statement))))

(defn arithm
  "similar to choco arithm. lets you use division with an IntVar. other
  than that it is a shortcut for having a compare and operation in 1
  instruction. lets you write a = b + c. allowed operators are
  #{+ * / -}, allowed comparisons are #{= > < != not= >= <=}
  a, b and c are allowed to be partial constraints"
  {:choco ["arithm(IntVar var,  String op,  int cste)"
           "arithm(IntVar var1, String op,  IntVar var2)"
           "arithm(IntVar var1, String op1, IntVar var2, String op2, int cste)"
           "arithm(IntVar var1, String op1, IntVar var2, String op2, IntVar var3)"]}
  ([a compare b]
   {:pre [(comparison-operator? compare)]}
   (let [compare (to-operator compare)]
     (-> (constraint ['arithm [a compare (preserve-consts b)]])
         (with-compiler compiler))))

  ([a compare b op c]
   {:pre [(comparison-operator? compare)
          (arithmetic-operator? op)]}
   (let [op (to-operator op)
         compare (to-operator compare)]
     (-> (constraint ['arithm [a compare b op (preserve-consts c)]])
         (with-compiler compiler)))))
