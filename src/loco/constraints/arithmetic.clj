(ns loco.constraints.arithmetic
  (:use loco.utils
        loco.constraints.utils)
  (:require [clojure.core.match :refer [match]]))

;;TODO: implement below functions
;; square(IntVar var1, IntVar var2)

(defn square
  "Creates a square constraint: result = dependency^2"
  [result dependency]
  [:constraint [:square [result dependency]]])

(defn neg
  "takes a partial constraint and creates a negative constraint from
  it (neg (- :x :b)) also can be used to create a neg var
  via (neg :-i :i)
  "
  ([label dependency]
   {:pre [(keyword? label) (keyword? dependency)]}
   ^{:neg dependency} [:var label :proto])
  ([dependency]
   [:constraint :partial [:neg dependency]]))

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
   {:pre [(comparison-operator? compare)]}
   [:constraint [:arithm [a compare (preserve-consts b)]]])

  ([a compare b op c]
   {:pre [(comparison-operator? compare) (arithmetic-operator? op)]}
   [:constraint [:arithm [a compare b op (preserve-consts c)]]]))

;;in clojure these are actually able to tell if the args are sorted...
(defn <
  "Constrains that X < Y"
  [x y]
  (arithm x :< y))

(defn >
  "Constrains that X > Y"
  [x y]
  (arithm x :> y))

(defn <=
  "Constrains that X <= Y"
  [x y]
  (arithm x :<= y))

(defn >=
  "Constrains that X >= Y"
  [x y]
  (arithm x :>= y))

(defn all-equal [vars]
  {:pre [(vector? vars)]}
  [:constraint [:all-equal vars]])

(defn =
  "Constrains that X = Y."
  {:choco "allEqual(IntVar... vars)"}
  [& more]
  (let [morev (vec more)]
    (match [morev]
           [[x y]] (arithm x := y)
           :else   (all-equal morev))))

(defn not-all-equal [vars]
  {:pre [(vector? vars)]}
  [:constraint [:not-all-equal vars]])

(defn not=
  "Constrains that X != Y, i.e. (not X = Y = ...)"
  {:choco "notAllEqual(IntVar... vars)"}
  [& more]
  (let [morev (vec more)]
    (match [morev]
           [[x y]] (arithm x :!= y)
           :else   (not-all-equal morev))))

(def != not=)

;;;;;; ARITHMETIC
(defn -
  "Takes a combination of int-vars and numbers, and returns another number/int-var which is constrained
  to equal (x - y - z - ...)"
  ([& args]
   [:constraint :partial [:- (vec args)]]))

(defn sum
  "Creates a sum constraint. Enforces that ∑i in |vars|varsi operator sum
  Creates a constraint summing elements of set sum{i | i in set} = sum"
  {:choco ["sum(IntVar[] vars, String operator, IntVar sum)"
           "sum(SetVar set, IntVar sum)"]
   :partial true}
  ([vars]
   {:pre [(sequential? vars)]}
   [:constraint :partial [:+ vars]]) ;; this is named differently
                                       ;; because it creates nice var
                                       ;; names. gets converted into a
                                       ;; :sum at compile step

  ([summation set-var]
   [:constraint [:sum [summation := set-var]]])

  ([summation operator vars]
   {:pre [(sequential? vars) (comparison-operator? operator)]}
   [:constraint [:sum [summation operator vars]]]))

(defn +
  "Takes a combination of int-vars and numbers, and returns another
  number/int-var which is constrained to equal the sum."
  ([& args]
   (sum (vec args))))

(defn *
  "Takes two arguments. One of the arguments can be a number greater than or equal to -1."
  [& args]
  (match (vec args)
         [x y] [:constraint :partial [:* [x y]]]
         [x & more] [:constraint :partial [:* [x (apply * more)]]]))

(defn div
  "Creates an euclidean division constraint. Ensures dividend / divisor
  = result, rounding towards 0 Also ensures divisor != 0"
  {:choco "div(IntVar dividend, IntVar divisor, IntVar result)"}
  ([dividend, divisor]
   [:constraint :partial [:/ [dividend, divisor]]])
  ([dividend, divisor, result]
   [:constraint [:div [result := dividend :/ divisor]]]))

(defn times
  "Creates a multiplication constraint: X * Y = Z, they can all be
  IntVars. seems similar to arithm... you should probably use arithm
  instead, for readability"
  {:choco "times(IntVar X, IntVar Y, IntVar Z)"}
  [x y z]
  [:constraint [:times [z := x :* y]]])

(defn min
  "The minimum of several arguments. The arguments can be a mixture of int-vars and numbers."
  {:choco "min(IntVar min, IntVar[] vars)"}
  [& more]
  (let [morev (vec more)]
    (match
     morev
     [(result :guard keyword?) (vars :guard vector?)] [:constraint [:min [result :of vars]]]
     [(vars :guard vector?)]                          [:constraint :partial [:min vars]]
     [& vars]                                         [:constraint :partial [:min (vec vars)]])))


(defn- max-partial [& vars]
  [:constraint :partial [:max (vec vars)]])

(defun max
  "The minimum of several arguments. The arguments can be a mixture of int-vars and numbers
  Creates a constraint over the maximum element in a set: max{i | i in set} = maxElementValue
  Creates a constraint over the maximum element induces by a set: max{weights[i-offset] | i in indices} = maxElementValue"
  {:choco
   ["max(IntVar max, IntVar[] vars)"
    "max(SetVar set, IntVar maxElementValue, boolean notEmpty)"
    "max(SetVar indices, int[] weights, int offset, IntVar maxElementValue, boolean notEmpty)"]}
  ([(max-list :guard sequential?)] (apply max-partial max-list))
  ([max (vars :guard sequential?)]
   [:constraint [:max [max [:of (vec vars)]]]])
  ([set-var max (not-empty? :guard boolean?)]
   [:constraint [:max [max [:of set-var] [:not-empty? not-empty?]]]])
  ([set-indices,
    (weights :guard [sequential? (p every? integer?)])
    (offset :guard integer?)
    max,
    (not-empty? :guard boolean?)]
   [:constraint [:max [max
                       [:of (preserve-consts (vec weights))]
                       [:indices set-indices]
                       [:offset (preserve-consts offset)]
                       [:not-empty? not-empty?]]]])
  ([& int-vars] (apply max-partial int-vars)))

(defn mod
  "Creates a modulo constraint. Ensures X % Y = Z"
  {:choco "mod(IntVar X, IntVar Y, IntVar Z)"}
  ([x y z]
   [:constraint [:mod [z := x :% y]]])
  ([x y]
   [:constraint :partial [:% [x y]]]))

(def % (partial mod))

(defn abs
  "Creates an absolute value constraint: abs-val = |var|"
  {:choco "absolute(IntVar var1, IntVar var2)"}
  ([var]
   [:constraint :partial [:abs [var]]])
  ([abs-val var]
   [:constraint [:abs [abs-val := var]]]))

;; one issue here is that the coeffs need to remain as ints, not as IntVars
(defn scalar
  "Creates a scalar constraint which ensures that Sum(vars[i]*coeffs[i]) operator scalar"
  {:choco "scalar(IntVar[] vars, int[] coeffs, String operator, IntVar scalar)"}
  ([vars coeffs]
   {:pre [(every? integer? coeffs)]}
   [:constraint :partial [:scalar [vars (preserve-consts coeffs)]]])

  ([scalar operator vars coeffs]
   {:pre [(comparison-operator? operator)
          (every? integer? coeffs)]}
   [:constraint [:scalar [scalar operator vars (preserve-consts coeffs)]]]))

;; distance(IntVar var1, IntVar var2, String op, int cste)
;; distance(IntVar var1, IntVar var2, String op, IntVar var3)
