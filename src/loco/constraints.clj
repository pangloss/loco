(ns loco.constraints
  (:require [
             clojure.core.match :refer [match]
             ]
            loco.automata)
  (:import org.chocosolver.solver.constraints.nary.automata.FA.FiniteAutomaton))

;;TODO: get rid of this... it's only used for the memoization part,
;;most likely that part of the code will be changed or removed
(defn- id
  []
  (gensym "id"))

;;;;; VAR GENERATION
;;FIXME: we don't have any tests in core-test that need this.
;; tests in sudoku do use this feature
(defn- valid-var-name? [var-name]
  (match [var-name]
         [(name :guard keyword?)] true
         [[(name :guard keyword?)]] true
         :else false))

(defn $const [var-name value]
  {:pre [(number? value)]}
  [:var var-name :hidden [:const value]])

(defn $in
  "Declares that a variable must be in a certain domain.
   Possible arglist examples:
   ($in :x 1 5)
   ($in :x [1 2 3 4 5])
   ($in :x 1 5 :bounded)"
  ([var-name lb ub bounded?]
   {:pre [(number? lb) (number? ub) (or (boolean? bounded?) (= bounded? :bounded))]}
   (if bounded?
     [:var var-name :public [:int lb ub :bounded]]
     ($in var-name lb ub)))

  ([var-name lb ub]
   [:var var-name :public [:int lb ub]])

  ([var-name values-or-const]
   {:pre [(or (number? values-or-const)
              (vector? values-or-const))]}

   (if (number? values-or-const)
     ($const var-name values-or-const)
     [:var var-name :public [:int (vec (sort values-or-const))]])))

(def $in-
  (comp (partial replace {:public :hidden}) (partial $in)))

(defn $neg
  "takes a partial constraint and creates a negative constraint from it ($neg ($- :x :b))

  also can be used to create a neg var via ($neg :-i :i)
  "
  ([label dependency]
   {:pre [(keyword? label) (keyword? dependency)]}
   ^{:neg dependency} [:var label :proto])
  ([dependency]
   [:constraint :partial [:neg dependency]]))


;;;;; CONSTRAINT GENERATION


;;;;; EQUALITY/COMPARISON
(def ^:private eq-converse
  {:= :=
   :< :>
   :> :<
   :<= :>=
   :>= :<=
   :!= :!=})

(declare $arithm)
;;in clojure these are actually able to tell if the args are sorted...
(defn $<
  "Constrains that X < Y"
  [x y]
  ($arithm x :< y))

(defn $>
  "Constrains that X > Y"
  [x y]
  ($arithm x :> y))

(defn $<=
  "Constrains that X <= Y"
  [x y]
  ($arithm x :<= y))

(defn $>=
  "Constrains that X >= Y"
  [x y]
  ($arithm x :>= y))

(defn $=
  "Constrains that X = Y."
  [& more]
  [:constraint [:all-equal (vec more)]])

(defn $not=
  "Constrains that X != Y, i.e. (not X = Y = ...)"
  [& more]
  [:constraint [:all-not-equal (vec more)]])

(defn $!=
  "Constrains that X != Y, i.e. (not X = Y = ...)"
  ([& more]
   (apply $not= more)))

;;;;;; ARITHMETIC
(defn $-
  "Takes a combination of int-vars and numbers, and returns another number/int-var which is constrained
  to equal (x - y - z - ...)"
  ([& args]
   [:constraint :partial [:- (vec args)]]))

;;;;;; TODO: convert below functions to new form
(defn $+
  "Takes a combination of int-vars and numbers, and returns another number/int-var which is constrained to equal the sum."
  ([& args]
   [:constraint :partial [:+ (vec args)]]))

;;FIXME: this should act similar to the * operator in clojure
(defn $*
  "Takes two arguments. One of the arguments can be a number greater than or equal to -1."
  [x y]
  [:constraint :partial [:* [x y]]])

(defn $div
  "Creates an euclidean division constraint. Ensures dividend / divisor
  = result, rounding towards 0 Also ensures divisor != 0"
  ([dividend, divisor]
   [:constraint :partial [:/ [dividend, divisor]]])
  ([dividend, divisor, result]
   [:constraint [:div [result := dividend :/ divisor]]]))

(defn $times
  "Creates a multiplication constraint: X * Y = Z, they can all be
  IntVars. seems similar to arithm... you should probably use $arithm
  instead, for readability"
  [x y z]
  [:constraint [:times [z := x :* y]]])

(def is-compare? #{:= :> :< :!= :>= :<=})
(def is-op? #{:+ :* :/ :-})

(defn $arithm
  "similar to choco arithm. lets you use division with an IntVar. other
  than that it is a shortcut for having a compare and operation in 1
  instruction. lets you write a = b + c. allowed operators are
  #{:+ :* :/ :-}, allowed comparisons are #{:= :> :< :!= :>= :<=}
  a, b and c are allowed to be partial constraints"
  ([a compare b]
   {:pre [(is-compare? compare)]}
   [:constraint [:arithm [a compare b]]])

  ([a compare b op c]
   {:pre [(is-compare? compare) (is-op? op)]}
   [:constraint [:arithm [a compare b op c]]]))

(defn $min
  "The minimum of several arguments. The arguments can be a mixture of int-vars and numbers."
  [& args]
  {:type :min
   :args args
   :id (id)
   :eq-shortcut true})

(defn $max
  "The maximum of several arguments. The arguments can be a mixture of int-vars and numbers."
  [& args]
  {:type :max
   :args args
   :id (id)
   :eq-shortcut true})


(defn $mod
  "Creates a modulo constraint. Ensures X % Y = Z"
  ([x y z]
   [:constraint [:mod [z := x :% y]]])
  ([x y]
   [:constraint :partial [:% [x y]]]))

(def $% (partial $mod))


(defn $abs
  "Given a variable X, returns the absolute value of X, or |X|."
  [X]
  {:type :abs
   :arg X
   :id (id)
   :eq-shortcut true})


(defn $scalar
  "Given a list of variables X, Y, Z, etc. and a list of number coefficients a, b, c, etc. returns a new variable constrained to equal aX + bY + cZ ..."
  [variables coefficients]
  {:type :scalar
   :variables variables
   :coefficients coefficients
   :id (id)
   :eq-shortcut true})

;;;;; LOGIC

(defn $true
  "Always true."
  []
  {:type :true
   :id (id)})


(defn $false
  "Always false."
  []
  {:type :false
   :id (id)})

(defn $and
  "An \"and\" statement (i.e. \"P^Q^...\"); this statement is true if and only if every subconstraint is true."
  [& constraints]
  (if (empty? constraints)
    ($true)
    {:type :and, :constraints constraints}))

(defn $or
  "An \"or\" statement (i.e. \"PvQv...\"); this statement is true if and only if at least one subconstraint is true."
  [& constraints]
  (if (empty? constraints)
    ($false)
    {:type :or, :constraints constraints}))

(defn $not
  "Given a constraint C, returns \"not C\" a.k.a. \"~C\", which is true iff C is false."
  [C]
  {:type :not, :arg C})

(defn $if
  "An \"if\" statement (i.e. \"implies\", \"P=>Q\"); this statement is true if and only if P is false or Q is true.
In other words, if P is true, Q must be true (otherwise the whole statement is false).
An optional \"else\" field can be specified, which must be true if P is false."
  ([if-this then-this]
    {:type :if
     :if if-this
     :then then-this})
  ([if-this then-this else-this]
    {:type :if
     :if if-this
     :then then-this
     :else else-this}))

(defn $cond
  "A convenience function for constructing a \"cond\"-like statement out of $if statements.
The final \"else\" can be specified by itself (being the odd argument) or with the :else keyword.

Example:
($cond P Q, R S, :else T)
=> ($if P Q ($if R S T))

If no \"else\" clause is specified, it is \"True\" by default."
  [& clauses]
  (cond
    (empty? clauses) ($true)
    (empty? (rest clauses)) (first clauses)
    (empty? (rest (rest clauses))) (if (= (first clauses) :else)
                                     (second clauses)
                                     (apply $if clauses))
    :else ($if (first clauses) (second clauses) (apply $cond (rest (rest clauses))))))

(defn $reify
  "Given a constraint C, will generate a bool-var V such that (V = 1) iff C."
  [C]
  {:type :reify
   :arg C
   :id (id)})

;;;;; GLOBAL

(defn $distinct
  "Given a bunch of int-vars, ensures that all of them have different values, i.e. no two of them are equal."
  [vars]
  {:type :distinct
   :args vars})

(defn $all-different?
  "Deprecated: use $distinct"
  [& vars]
  ($distinct vars))

(defn $circuit
  "Given a list of int-vars L, and an optional offset number (default 0), the elements of L define a circuit, where
(L[i] = j + offset) means that j is the successor of i.
Hint: make the offset 1 when using a 1-based list."
  ([list-of-vars]
    ($circuit list-of-vars 0))
  ([list-of-vars offset]
    {:type :circuit
     :vars list-of-vars
     :offset offset}))

(defn $nth
  "Given a list of int-vars L, an int-var i, and an optional offset number (default 0), returns a new int-var constrained to equal L[i], or L[i - offset]."
  ([list-of-vars index-var]
    ($nth list-of-vars index-var 0))
  ([list-of-vars index-var offset]
    {:type :nth
     :vars list-of-vars
     :index index-var
     :offset offset
     :id (id)
     :eq-shortcut true}))

(defn $regular
  "Takes a Choco automaton object constructed by the loco.automata
  namespace, and constrains that a list of variables represents an
  input string accepted by the automaton."
  [^FiniteAutomaton automaton list-of-vars]
  {:type :regular
   :list-of-vars list-of-vars
   :automaton automaton})

(defn $cardinality
  "Takes a list of variables, and a frequency map (from numbers to frequencies), constrains
that the frequency map is accurate. If the :closed flag is set to true, any keys that aren't
in the frequency map can't appear at all in the list of variables.

Example: ($cardinality [:a :b :c :d :e] {1 :ones, 2 :twos} :closed true)
=> {:a 1, :b 1, :c 2, :d 2, :e 2
    :ones 2, :twos 3}"
  [variables frequencies & {:as args}]
  {:type :cardinality
   :variables variables
   :values (keys frequencies)
   :occurrences (vals frequencies)
   :closed (:closed args)})

(defn $knapsack
  "Takes constant weights / values for a list of pre-defined items, and
a list of variables representing the amount of each item. Constrains that
the values of all the items add up to the total-value, while the items'
weights add up to total-weight.

Example: ($knapsack [3 1 2]    ; weights
                    [5 6 7]    ; values
                    [:x :y :z] ; occurrences
                    :W         ; total weight
                    :V)        ; total value"
  [weights values occurrences total-weight total-value]
  (assert (and (every? integer? weights)
               (every? integer? values))
          "$knapsack: weights and values must be collections of constant integers")
  {:type :knapsack
   :weights weights
   :values values
   :occurrences occurrences
   :total-weight total-weight
   :total-value total-value})
