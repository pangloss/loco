(ns loco.constraints)

(def imports
  [
   "constraints/vars"
   "constraints/views/minus"
   "constraints/views/offset"
   "constraints/views/scale"
   "constraints/views/abs"
   
   "constraints/sum"

   "constraints/times"
   "constraints/arithm"
   "constraints/arithmetic/subtraction"

   "constraints/mod"
   "constraints/abs"
   "constraints/div"
   "constraints/all_equal"
   "constraints/not_all_equal"

   "constraints/all_different"
   "constraints/all_different_except_0"
   "constraints/among"
   "constraints/at_least_n_values"
   "constraints/at_most_n_values"
   "constraints/bin_packing"
   "constraints/bits_int_channeling"
   "constraints/bools_int_channeling"
   "constraints/cardinality"
   "constraints/circuit"
   "constraints/clauses_int_channeling"
   "constraints/count"
   "constraints/cumulative"
   "constraints/diff_n"

   ;;TODO: ;;constraints/distance
   "constraints/element"
   "constraints/int_value_precede_chain"
   "constraints/inverse_channeling"
   "constraints/knapsack"
   "constraints/lex_chain_less"
   "constraints/lex_chain_less_equal"
   "constraints/lex_less"
   "constraints/lex_less_equal"
   "constraints/max"
   "constraints/member"
   "constraints/min"
   "constraints/n_values"
   "constraints/not_member"
   "constraints/nth"
   "constraints/path"
   "constraints/scalar"
   "constraints/sort"
   "constraints/square"
   "constraints/sub_circuit"
   "constraints/sub_path"
   "constraints/table"
   "constraints/tree"

   "constraints/set/intersection"
   "constraints/set/union"
   "constraints/set/nb_empty"
   "constraints/set/not_empty"
   "constraints/set/off_set"
   "constraints/set/partition"
   "constraints/set/subset_eq"
   "constraints/set/sum_elements"
   "constraints/set/symetric"
   "constraints/set/all_disjoint"
   "constraints/set/disjoint"
   "constraints/set/set_bools_channeling"
   "constraints/set/sets_ints_channeling"
   "constraints/set/inverse"

   "constraints/logic/logic"
   "constraints/automata/regular"
   ])

(apply load imports)

;; -------------------- MDD --------------------
;; mddc(IntVar[] vars, MultivaluedDecisionDiagram MDD)
;; requires building a complex object of ints and tuples
;;http://www.choco-solver.org/apidocs/org/chocosolver/util/objects/graphs/MultivaluedDecisionDiagram.html


;;TODO: implement allDifferentUnderCondition(IntVar[] vars, Condition condition, boolean singleCondition)
;;TODO: figure out how to convert a function into a Condition object
;;http://www.choco-solver.org/apidocs/org/chocosolver/solver/constraints/nary/alldifferent/conditions/Condition.html
;;possibly need to use reify
#_(defloco $distinct-under-condidiont
    "Creates an allDifferent constraint subject to the given
  condition. More precisely: IF singleCondition for all X,Y in vars,
  condition(X) => X != Y ELSE for all X,Y in vars, condition(X) AND
  condition(Y) => X != Y"
    {:choco "allDifferentUnderCondition(IntVar[] vars, Condition condition, boolean singleCondition)"}
    [int-vars condition single-condition?]
    {:pre [(sequential? int-vars)
           (boolean? single-condition?)
           (fn? condition)]}
    )


;;TODO: key-sort implementation requires int-var[][]
#_(defloco $key-sort
    "Creates a keySort constraint which ensures that the variables of
  SORTEDvars correspond to the variables of vars according to a
  permutation stored in PERMvars (optional, can be null). The variables
  of SORTEDvars are also sorted in increasing order wrt to K-size
  tuples. The sort is stable, that is, ties are broken using the
  position of the tuple in vars.


For example:
- vars= (<4,2,2>,<2,3,1>,<4,2,1><1,3,0>)
- SORTEDvars= (<1,3,0>,<2,3,1>,<4,2,2>,<4,2,1>)
- PERMvars= (2,1,3,0)
- K = 2"
    {:choco "keySort(IntVar[][] vars, IntVar[] PERMvars, IntVar[][] SORTEDvars, int K)"}
    [])
