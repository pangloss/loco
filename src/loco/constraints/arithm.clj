(ns loco.constraints.arithm
  (:require
   [clojure.core.match :refer [match]]
   [clojure.spec.alpha :as s]
   [clojure.walk :as walk]
   [loco.constraints.utils
    :refer [constraint defloco comparison-operator? comparison-symbol? to-operator
            arithmetic-symbol? int-var? int-or-intvar? coerce-int-var arithmetic-operator?]
    :as utils]
   [loco.match :refer [match+]]
   [loco.utils :refer [p c]]
   )
  (:import
   [org.chocosolver.solver.variables BoolVar IntVar]))

(def ^:private constraint-name 'arithm)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args (s/or
                :compare (s/cat :eq-var ::utils/coerce-intvar?
                                :compare-op ::utils/comparison-symbol?
                                :operand ::utils/int-or-intvar?)
                :arithm (s/cat :eq-var ::utils/coerce-intvar?
                               :compare-op ::utils/comparison-symbol?
                               :operand1 ::utils/coerce-intvar?
                               :arithm-op ::utils/arithmetic-symbol?
                               :operand2 ::utils/int-or-intvar?))))

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:args [:compare {:eq-var eq-var :compare-op op, :operand var}]}
           (.arithm model (coerce-int-var model eq-var) (name op) var)

           {:args [:arithm {:eq-var eq-var :compare-op op, :operand1 var
                            :arithm-op op2 :operand2 var2}]}
           (.arithm model
                    (coerce-int-var model eq-var) (name op)
                    (coerce-int-var model var) (name op2) var2)

           ::s/invalid
           (utils/report-spec-error constraint-name ::compile-spec var-subed-statement))))

(defloco $arithm
  "Creates an arithmetic constraint:
  eq op1 operand1 op2 operand2,

  eq = IntVar
  operand1 = IntVar
  operand2 = IntVar | integer
  op1 in #{= > < != not= >= <=}
  op2 in #{+ * / -}"
  {:choco ["arithm(IntVar var,  String op,  int cste)"
           "arithm(IntVar var1, String op,  IntVar var2)"
           "arithm(IntVar var1, String op1, IntVar var2, String op2, int cste)"
           "arithm(IntVar var1, String op1, IntVar var2, String op2, IntVar var3)"]}
  ([a compare b]
   {:pre [(comparison-operator? compare)]}
   (let [compare (to-operator compare)]
     (cond
       (and (= a b)
            (= '= compare)) nil
       :else (constraint constraint-name
                         [a compare b]
                         compiler))))

  ([a compare b op c]
   {:pre [(comparison-operator? compare)
          (arithmetic-operator? op)]}
   (let [op (to-operator op)
         compare (to-operator compare)]
     (constraint constraint-name
                 [a compare b op c]
                 compiler))))

;;FIXME: not sure how to get this working :(
;;maybe the defloco should be done sorta like a safer reset-meta! like in vars.clj
(s/fdef loco.constraints/$arithm
  :args (s/or
         :arity-3 (s/cat :a any? :compare ::utils/comparison-operator? :b any?)
         :arity-5 (s/cat :a any?
                         :compare ::utils/comparison-operator?
                         :b any?
                         :op ::utils/arithmetic-operator?
                         :c any?)
         )
  :ret vector?
  )

(defloco $<
  "Constrains that X < Y"
  [x y]
  ($arithm x < y))

(defloco $>
  "Constrains that X > Y"
  [x y]
  ($arithm x > y))

(defloco $<=
  "Constrains that X <= Y"
  [x y]
  ($arithm x <= y))

(defloco $>=
  "Constrains that X >= Y"
  [x y]
  ($arithm x >= y))
