;;TODO: apply these to meta data of functions as completed
;; boolsIntChanneling(BoolVar[] bVars, IntVar var, int offset)
;; circuit(IntVar[] vars, int offset, CircuitConf conf)
;; clausesIntChanneling(IntVar var, BoolVar[] eVars, BoolVar[] lVars)
;; costRegular(IntVar[] vars, IntVar cost, ICostAutomaton costAutomaton)
;; cumulative(Task[] tasks, IntVar[] heights, IntVar capacity)
;; cumulative(Task[] tasks, IntVar[] heights, IntVar capacity, boolean incremental)
;; cumulative(Task[] tasks, IntVar[] heights, IntVar capacity, boolean incremental, Cumulative.Filter... filters)
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
;; allDifferent(IntVar[] vars, String CONSISTENCY)
;; allDifferentUnderCondition(IntVar[] vars, Condition condition, boolean singleCondition)

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

(defn $bool- [var-name]
  [:var var-name :hidden [:bool 0 1]])

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
     ($const var-name values-or-const))))

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
  {:choco ["and(BoolVar... bools)"
           "and(Constraint... cstrs)"]}
  [& constraints-or-bools]
  {:pre [(coll? constraints-or-bools) (not (empty? constraints-or-bools))]}
  [:constraint [:and (vec constraints-or-bools)]])

;;TODO: there is also a boolean list form that can be useful to implement
(defn $or
  "An \"or\" statement (i.e. \"PvQv...\"); this statement is true if and
  only if at least one subconstraint is true."
  {:choco ["or(BoolVar... bools)"
           "or(Constraint... cstrs)"]}
  [& constraints-or-bools]
  {:pre [(coll? constraints-or-bools) (not (empty? constraints-or-bools))]}
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

(def $all-different-except-0 $distinct-except-0)

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
  "Creates a knapsack constraint. Ensures that :
  - occurrences[i] * weight[i] = weightSum
  - occurrences[i] * energy[i] = energySum
  and maximizing the value of energySum.

  Example: ($knapsack [3 1 2]    ; weight of item
                      [5 6 7]    ; energy of item
                      [:x :y :z] ; occurrences
                      :W         ; total weight
                      :V)        ; total energy"
  {:choco "knapsack(IntVar[] occurrences, IntVar weightSum, IntVar energySum, int[] weight, int[] energy)"}
  [weight energy occurrences weight-sum energy-sum]
  {:pre [
         (every? integer? weight)
         (every? integer? energy)
         (every? keyword? occurrences)
         (every? (p <= 0) weight) ;;all items in weight or energy must be above 1
         (every? (p <= 0) energy)
         ]}
  [:constraint
   [:knapsack
    [
     [:weight (preserve-consts weight)]
     [:energy (preserve-consts energy)]
     [:occurrences occurrences]
     [:weight-sum weight-sum]
     [:energy-sum energy-sum]
     ]]])

(defn $member
  "Creates a member constraint. Ensures var takes its values in [LB, UB]
   Creates a member constraint. Ensures var takes its values in table"
  {:choco ["member(IntVar var, int[] table)"
           "member(IntVar var, int lb, int ub)"]}
  ([var table]
   {:pre [(coll? table)
          (every? integer? table)]}
   [:constraint [:member [var [:table (preserve-consts (vec table))]]]])

  ([var lb ub]
   {:pre [(integer? lb) (integer? ub) (< lb ub)]}
   [:constraint [:member [var
                          [:lower-bound (preserve-consts lb)]
                          [:upper-bound (preserve-consts ub)]]]]))

(defn $not-member
  "Creates a member constraint. Ensures var does not take its values in [LB, UB]
   Creates a member constraint. Ensures var does not take its values in table"
  {:choco ["notMember(IntVar var, int[] table)"
           "notMember(IntVar var, int lb, int ub)"]}
  ([var table]
   {:pre [(coll? table)
          (every? integer? table)]}
   [:constraint [:not-member [var [:table (preserve-consts (vec table))]]]])

  ([var lb ub]
   {:pre [(integer? lb) (integer? ub) (< lb ub)]}
   [:constraint [:not-member [var
                              [:lower-bound (preserve-consts lb)]
                              [:upper-bound (preserve-consts ub)]]]]))

(defn $n-values
  "Creates an nValue constraint. Let N be the number of distinct values
  assigned to the variables of the vars collection. Enforce condition
  N = nValues to hold."
  {:choco "nValues(IntVar[] vars, IntVar nValues)"}
  [vars n-values]
  {:pre [(coll? vars)]}
  [:constraint [:n-values [(vec vars) n-values]]])

(defn $sort
  "Creates a sort constraint which ensures that the variables of
  sorted-vars correspond to the variables of vars according to a
  permutation. The variables of sortedVars are also sorted in increasing
  order.

  For example:
  - X= (4,2,1,3)
  - Y= (1,2,3,4)"
  {:choco "sort(IntVar[] vars, IntVar[] sortedVars)"}
  [vars sorted-vars]
  {:pre [(coll? vars) (coll? sorted-vars)]}
  [:constraint [:sort [(vec vars) (vec sorted-vars)]]])


(defn $count
  "Creates a count constraint. Let N be the number of variables of the
  vars collection assigned to value value; Enforce condition N = limit
  to hold. "
  {:choco ["count(int value, IntVar[] vars, IntVar limit) "
           "count(IntVar value, IntVar[] vars, IntVar limit)"]}
  [value vars limit]
  {:pre [(coll? vars)]}
  [:constraint [:count [(vec vars) [:value (preserve-consts value)] [:limit limit]]]])

(defn $among
  "Creates an among constraint. nbVar is the number of variables of the
  collection vars that take their value in values."
  {:choco "among(IntVar nbVar, IntVar[] vars, int[] values)"
   :gccat "http://www.emn.fr/x-info/sdemasse/gccat/Camong.html"}
  [nb-var vars values]
  {:pre [(coll? vars) (coll? values) (every? integer? values)]}
  [:constraint [:among [(vec vars) [:nb-var nb-var] [:values (preserve-consts (vec values))]]]])

(defn $at-least-n-values
  "Creates an atLeastNValue constraint. Let N be the number of distinct
  values assigned to the variables of the vars collection. Enforce
  condition N >= nValues to hold."
  {:choco "atLeastNValues(IntVar[] vars, IntVar nValues, boolean AC)"}
  ([vars n-values] ($at-least-n-values vars n-values false))
  ([vars n-values ac]
   {:pre [(coll? vars) (boolean? ac)]}
   [:constraint [:at-least-n-values [(vec vars) [:n-values n-values] [:ac ac]]]]))

(defn $at-most-n-values
  "Creates an atMostNValue constraint. Let N be the number of distinct
  values assigned to the variables of the vars collection. Enforce
  condition N <= nValues to hold."
  {:choco "atMostNValues(IntVar[] vars, IntVar nValues, boolean STRONG)"}
  ([vars n-values] ($at-most-n-values vars n-values false))
  ([vars n-values strong]
   {:pre [(coll? vars) (boolean? strong)]}
   [:constraint [:at-most-n-values [(vec vars) [:n-values n-values] [:strong strong]]]]))

(defn $bin-packing
  "Creates a BinPacking constraint. Bin Packing formulation:
  forall b in [0, binLoad.length - 1],
  binLoad[b] = sum(itemSize[i] |
  i in [0, itemSize.length - 1],
  itemBin[i] = b + offset forall i in [0, itemSize.length - 1],
  itemBin is in [offset, binLoad.length-1 + offset]

  Parameters:
  itemBin  - IntVar representing the bin of each item
  itemSize - int representing the size of each item
  binLoad  - IntVar representing the load of each bin (i.e. the sum of the size of the items in it)
  offset   - 0 by default but typically 1 if used within MiniZinc (which counts from 1 to n instead of from 0 to n-1)

  GCCAT:
  Given several items of the collection ITEMS (each of them
  having a specific weight), and different bins described the the
  items of collection BINS (each of them having a specific capacity
  capa), assign each item to a bin so that the total weight of the
  items in each bin does not exceed the capacity of the bin."
  {:choco "binPacking(IntVar[] itemBin, int[] itemSize, IntVar[] binLoad, int offset)"
   :gccat "http://sofdem.github.io/gccat/gccat/Cbin_packing_capa.html"
   :constraint-type [:resource-constraint]}
  ([item-map bin-load] {:pre [(map? item-map)]}
   ($bin-packing (keys item-map) (vals item-map) bin-load))
  ([item-bin, item-size, bin-load] ($bin-packing item-bin item-size bin-load 0))
  ([item-bin, item-size, bin-load, offset]
   {:pre [(coll? item-bin)
          (< 0 (count item-bin))
          (distinct? item-bin)
          (coll? item-size)
          (every? integer? item-size)
          (every? pos? item-size)
          (= (count item-size) (count item-bin))
          (coll? bin-load)
          (integer? offset)
          (<= 0 offset)]}
   [:constraint [:bin-packing
                 [[:item-bin (vec item-bin)]
                  [:item-size (preserve-consts (vec item-size))]
                  [:bin-load (vec bin-load)]
                  [:offset (preserve-consts offset)]]]]))


#_(defn diff-n
  "Creates a diffN constraint. Constrains each rectangle[i], given by
  their origins X[i],Y[i] and sizes width[i], height[i], to be non-overlapping."
  {:choco "diffN(IntVar[] X, IntVar[] Y, IntVar[] width, IntVar[] height, boolean addCumulativeReasoning)"
   :gccat "http://sofdem.github.io/gccat/gccat/Cdiffn.html"}
  [x y width height add-cumulative-reasoning?]
  )


;;TODO: it could be interesting to generate the bit-vars
(defn $bits-channeling
  "Creates an channeling constraint between an integer variable and a set of bit variables. Ensures that var = 20*BIT_1 + 21*BIT_2 + ... 2n-1*BIT_n.
  BIT_1 is related to the first bit of OCTET (2^0), BIT_2 is related
  to the first bit of OCTET (2^1), etc.  The upper bound of var is
  given by 2n, where n is the size of the array bits."
  {:choco "bitsIntChanneling(BoolVar[] bits, IntVar var)"}
  [bits int-var]
  {:pre [(coll? bits)]}
  (-> []
      (into (mapv $bool bits))
      (into [[:constraint [:bit-channeling [(vec bits) int-var]]]])
      (with-meta {:generated-vars true})))
