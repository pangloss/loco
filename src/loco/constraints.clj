(ns loco.constraints
  (:use loco.utils
        loco.constraints.utils)
  (:require [clojure.core.match :refer [match]]
            loco.automata
            loco.constraints.arithmetic)
  (:import org.chocosolver.solver.constraints.nary.automata.FA.FiniteAutomaton))

(defn- inherit-def [prefix sym var-to-inherit]
  (let [sym (symbol (str prefix (name sym)))]
    (println "creating def" (str *ns* "/" (name sym)))
    (->
     (intern *ns* sym var-to-inherit)
     (alter-meta! ,,, merge (dissoc (meta var-to-inherit) :name)))))

(def ^:private to-inherit
  (->> ['loco.constraints.arithmetic]
       (map ns-publics)
       (into {})))

(doseq [[sym var] to-inherit]
  (inherit-def "$" sym var))

;;TODO: apply these to meta data of functions as completed
;; binPacking(IntVar[] itemBin, int[] itemSize, IntVar[] binLoad, int offset)
;; among(IntVar nbVar, IntVar[] vars, int[] values)
;; atLeastNValues(IntVar[] vars, IntVar nValues, boolean AC)
;; atMostNValues(IntVar[] vars, IntVar nValues, boolean STRONG)
;; count(int value, IntVar[] vars, IntVar limit)
;; count(IntVar value, IntVar[] vars, IntVar limit)
;; sort(IntVar[] vars, IntVar[] sortedVars)
;; member(IntVar var, int[] table)
;; member(IntVar var, int lb, int ub)
;; notMember(IntVar var, int[] table)
;; notMember(IntVar var, int lb, int ub)
;; nValues(IntVar[] vars, IntVar nValues)
;;
;; bitsIntChanneling(BoolVar[] bits, IntVar var)
;; boolsIntChanneling(BoolVar[] bVars, IntVar var, int offset)
;; circuit(IntVar[] vars, int offset, CircuitConf conf)
;; clausesIntChanneling(IntVar var, BoolVar[] eVars, BoolVar[] lVars)
;; costRegular(IntVar[] vars, IntVar cost, ICostAutomaton costAutomaton)
;; cumulative(Task[] tasks, IntVar[] heights, IntVar capacity)
;; cumulative(Task[] tasks, IntVar[] heights, IntVar capacity, boolean incremental)
;; cumulative(Task[] tasks, IntVar[] heights, IntVar capacity, boolean incremental, Cumulative.Filter... filters)
;; diffN(IntVar[] X, IntVar[] Y, IntVar[] width, IntVar[] height, boolean addCumulativeReasoning)
;; intValuePrecedeChain(IntVar[] X, int[] V)
;; intValuePrecedeChain(IntVar[] X, int S, int T)
;; inverseChanneling(IntVar[] vars1, IntVar[] vars2)
;; inverseChanneling(IntVar[] vars1, IntVar[] vars2, int offset1, int offset2)
;; keySort(IntVar[][] vars, IntVar[] PERMvars, IntVar[][] SORTEDvars, int K)
;; lexChainLess(IntVar[]... vars)
;; lexChainLessEq(IntVar[]... vars)
;; lexLess(IntVar[] vars1, IntVar[] vars2)
;; lexLessEq(IntVar[] vars1, IntVar[] vars2)
;; mddc(IntVar[] vars, MultivaluedDecisionDiagram MDD)
;; multiCostRegular(IntVar[] vars, IntVar[] costVars, ICostAutomaton costAutomaton)
;; path(IntVar[] vars, IntVar start, IntVar end)
;; path(IntVar[] vars, IntVar start, IntVar end, int offset)
;; subCircuit(IntVar[] vars, int offset, IntVar subCircuitLength)
;; subPath(IntVar[] vars, IntVar start, IntVar end, int offset, IntVar SIZE)
;; table(IntVar[] vars, Tuples tuples)
;; table(IntVar[] vars, Tuples tuples, String algo)
;; table(IntVar var1, IntVar var2, Tuples tuples)
;; table(IntVar var1, IntVar var2, Tuples tuples, String algo)
;; tree(IntVar[] succs, IntVar nbTrees)
;; tree(IntVar[] succs, IntVar nbTrees, int offset)
;; and(BoolVar... bools)
;; or(BoolVar... bools)
;; allDifferent(IntVar[] vars, String CONSISTENCY)
;; allDifferentUnderCondition(IntVar[] vars, Condition condition, boolean singleCondition)

;;;;; VAR GENERATION
;;FIXME: we don't have any tests in core-test that need this.
;; tests in sudoku do use this feature
(defn- valid-var-name? [var-name]
  (match [var-name]
         [(name :guard keyword?)] true
         [[(name :guard keyword?)]] true
         :else false))

(defn $const [var-name value]
  {:pre [(integer? value)]}
  [:var var-name :hidden [:const value]])

(defn $bool [var-name]
  [:var var-name :public [:bool 0 1]])

(defn $in
  "Declares that a variable must be in a certain domain.
   Possible arglist examples:
   ($in :x 1 5)
   ($in :x [1 2 3 4 5])
   ($in :x 1 5 :bounded)"
  ([var-name lb ub bounded?]
   {:pre [(integer? lb) (integer? ub) (or (boolean? bounded?) (= bounded? :bounded))]}
   (if bounded?
     [:var var-name :public [:int lb ub :bounded]]
     ($in var-name lb ub)))

  ([var-name lb ub]
   (match (sort [lb ub])
          [0 1] ($bool var-name)
          :else [:var var-name :public [:int lb ub]]))

  ([var-name values-or-const]
   {:pre [(or
           (integer? values-or-const)
           (and (coll? values-or-const)
                (every? integer? values-or-const))
           )]}
   (if (coll? values-or-const)
     (match
      [(vec (sort values-or-const))]
      [[0 1]] ($bool var-name)
      [domain] [:var var-name :public [:int domain]])
     ($const var-name values-or-const)
     )
   ))

(def $in-
  (comp (partial replace {:public :hidden}) (partial $in)))

(def $int $in)


;;;;; LOGIC

(defn $true
  "Always true."
  []
  [:constraint :true])

(defn $false
  "Always false."
  []
  [:constraint :false])

;;TODO: there is also a boolean list form that can be useful to implement
;;needs to be done at the compile phase, not here
(defn $and
  "An \"and\" statement (i.e. \"P^Q^...\"); this statement is true if
  and only if every subconstraint is true."
  {:choco "and(Constraint... cstrs)"}
  [& constraints-or-bools]
  {:pre [(not (empty? constraints-or-bools))]}
  [:constraint [:and (vec constraints-or-bools)]])

;;TODO: there is also a boolean list form that can be useful to implement
(defn $or
  "An \"or\" statement (i.e. \"PvQv...\"); this statement is true if and
  only if at least one subconstraint is true."
  {:choco "or(Constraint... cstrs)"}
  [& constraints-or-bools]
  {:pre [(not (empty? constraints-or-bools))]}
  [:constraint [:or (vec constraints-or-bools)]])

(defn $not
  "Given a constraint C, returns \"not C\" a.k.a. \"~C\", which is true iff C is false."
  {:choco "not(Constraint cstr)"}
  [constraint]
  [:constraint [:not constraint]])

(defn $when
  [if-this then-this]
  [:constraint [:when [if-this then-this]]])

(defn $if
  "An \"if\" statement (i.e. \"implies\", \"P=>Q\"); this statement is true if and only if P is false or Q is true.
In other words, if P is true, Q must be true (otherwise the whole
  statement is false).  An optional \"else\" field can be specified,
  which must be true if P is false."
  [if-this then-this else-this]
  [:constraint [:if-else [if-this then-this else-this]]])

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

;;TODO: not sure how this should be implemented
(defn $reify
  "Given a constraint C, will generate a bool-var V such that (V = 1) iff C."
  {:choco "reification(BoolVar var, Constraint cstr)"}
  [constraint]
  [:reify constraint])



;;TODO: organize functions better
;;;;; GLOBAL

(defn $distinct
  "Given a bunch of int-vars, ensures that all of them have different
  values, i.e. no two of them are equal."
  {:choco "allDifferent(IntVar... vars)"}
  [vars]
  {:pre [(vector? vars)]}
  [:constraint [:distinct vars]])

(def $all-different $distinct)

(defn $distinct-except-0
  "Creates an allDifferent constraint for variables that are not equal
  to 0. There can be multiple variables equal to 0."
  {:choco "allDifferentExcept0(IntVar[] vars)"}
  [vars]
  {:pre [(vector? vars)]}
  [:constraint [:distinct-except-0 vars]])

(defn $circuit
  "Given a list of int-vars L, and an optional offset number (default
  0), the elements of L define a circuit, where (L[i] = j + offset)
  means that j is the successor of i.  Hint: make the offset 1 when
  using a 1-based list."
  {:choco ["circuit(IntVar[] vars)"
           "circuit(IntVar[] vars, int offset)"]}
  ([vars]
    ($circuit vars 0))
  ([vars offset]
   {:pre [(integer? offset)]}
   [:constraint [:circuit [vars [:offset (preserve-consts offset)]]]]))

(defn $nth
  "partial for $element"
  {:choco "element(IntVar value, IntVar[] table, IntVar index, int offset)"}
  ([var-list index]
   ($nth var-list index 0))

  ([var-list index offset]
   {:pre [(integer? offset) (vector? var-list)]}
   (let [table (if (every? integer? var-list)
                 (preserve-consts var-list)
                 var-list)]

     [:constraint :partial [:$nth [table
                                   [:at index]
                                   [:offset (preserve-consts offset)]]]])))

(defn $element
  "Given a list of int-vars L, an int-var i, and an optional offset
  number (default 0), returns a new int-var constrained to equal L[i],
  or L[i - offset].

  value - an integer variable taking its value in table
  var-list - an array of integer values or variables
  index - an integer variable representing the value of value in table
  offset - offset matching index.lb and table[0] (Generally 0)"
  {:choco "element(IntVar value, IntVar[] table, IntVar index, int offset)"}
  ([value var-list index]
   ($element value var-list index 0))

  ([value var-list index offset]
   {:pre [(integer? offset) (vector? var-list)]}
   (let [table (if (every? integer? var-list)
                 (preserve-consts var-list)
                 var-list)]
     [:constraint [:element [value
                             [:in table]
                             [:at index]
                             [:offset (preserve-consts offset)]]]])))

(def $elem $element)

;;TODO: figure this out when we get to automata solution tests
(defn $regular
  "Takes a Choco automaton object constructed by the loco.automata
  namespace, and constrains that a list of variables represents an
  input string accepted by the automaton."
  {:choco "regular(IntVar[] vars, IAutomaton automaton)"}
  [^FiniteAutomaton automaton list-of-vars]
  {:type :regular
   :list-of-vars list-of-vars
   :automaton automaton})

(defn $cardinality
  "Takes a list of variables, and a frequency map (from numbers to
  frequencies), constrains that the frequency map is accurate. If
  the :closed flag is set to true, any keys that aren't in the
  frequency map can't appear at all in the list of variables.

  Example: ($cardinality [:a :b :c :d :e] {1 :ones, 2 :twos} :closed true)
  => {:a 1, :b 1, :c 2, :d 2, :e 2 :ones 2, :twos 3}"
  {:choco "globalCardinality(IntVar[] vars, int[] values, IntVar[] occurrences, boolean closed)"
   :gccat "http://sofdem.github.io/gccat/gccat/Cglobal_cardinality.html"}
  ([variables frequencies]
   ($cardinality variables frequencies false))

  ([variables frequencies closed?]
   {:pre [
          (map? frequencies)
          (vector? variables)
          (or (= closed? :closed) (boolean? closed?))
          (every? integer? (keys frequencies))
          (every? keyword? (vals frequencies))
          (distinct? (keys frequencies))
          (distinct? (vals frequencies))
          ]
    }
   (let [closed (case closed?
                  :closed true
                  closed?)
         values (preserve-consts (vec (keys frequencies)))
         occurences (vec (vals frequencies))]
     [:constraint
      [:cardinality
       [variables [values occurences] [:closed closed]]]])))

(defn $knapsack
  "Takes constant weights / values for a list of pre-defined items, and
  a list of variables representing the amount of each item. Constrains
  that the values of all the items add up to the total-value, while
  the items' weights add up to total-weight.

Example: ($knapsack [3 1 2]    ; weights
                    [5 6 7]    ; values
                    [:x :y :z] ; occurrences
                    :W         ; total weight
                    :V)        ; total value"
  {:choco "knapsack(IntVar[] occurrences, IntVar weightSum, IntVar energySum, int[] weight, int[] energy)"}
  [weights values occurrences total-weight total-value]
  {:pre [
         (every? integer? weights)
         (every? integer? values)
         (every? keyword? occurrences)
         (every? (p <= 0) weights)
         (every? (p <= 0) values)
         ]}
  [:constraint
   [:knapsack
    [
     [:weights (preserve-consts weights)]
     [:values (preserve-consts values)]
     [:occurrences occurrences]
     [:total-weight total-weight]
     [:total-value total-value]
     ]]])
