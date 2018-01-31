(ns loco.constraints
  (:refer-clojure :exclude [set sort partition count min max nth])
  (:use loco.utils
        loco.constraints.utils)
  (:require
   ;;loco.vars

   [loco.constraints.automata
    regular]
   ;;[loco.constraints.automata regular]

   loco.constraints.logic.logic

   [loco.constraints.set
    all-disjoint
    disjoint
    intersection
    inverse
    nb-empty
    not-empty
    off-set
    partition
    set-bools-channeling
    sets-ints-channeling
    subset-eq
    sum-elements
    symetric
    union
    ]

   [loco.constraints
    arithmetic
    times
    mod
    abs
    div
    all-equal
    not-all-equal

    ;;distance
    all-different
    all-different-except-0
    among
    at-least-n-values
    at-most-n-values
    bin-packing
    bits-int-channeling
    bools-int-channeling
    cardinality
    circuit
    clauses-int-channeling
    count
    cumulative
    diff-n
    element
    int-value-precede-chain
    inverse-channeling
    knapsack
    lex-chain-less
    lex-chain-less-equal
    lex-less
    lex-less-equal
    max
    member
    min
    n-values
    not-member
    nth
    path
    scalar
    sort
    square
    sub-circuit
    sub-path
    ;;sum
    table
    tree
    ]

   )
)

(defn- inherit-def [prefix dep-sym var-to-inherit]
  (let [sym (symbol (str prefix (name dep-sym)))]
    (if (qualified-symbol? sym)
      (println "qualified symbol error:" dep-sym)
      (when-not (and (< 1 (.length (name dep-sym)))
                     (.startsWith (name dep-sym) "*"))
        (do (println "creating def" (str *ns* "/" (name sym)))
            (->
             (intern *ns* sym var-to-inherit)
             (reset-meta! (meta var-to-inherit))))))))

(def ^:private to-inherit
  (->> [
        ;;'loco.vars
        'loco.constraints.arithmetic
        ;;'loco.constraints.sum
        'loco.constraints.arithm
        'loco.constraints.times
        'loco.constraints.mod
        'loco.constraints.abs
        'loco.constraints.div
        'loco.constraints.all-equal
        'loco.constraints.not-all-equal

        ;;TODO: write tests
        'loco.constraints.all-different
        'loco.constraints.all-different-except-0
        'loco.constraints.among
        'loco.constraints.at-least-n-values
        'loco.constraints.at-most-n-values
        'loco.constraints.bin-packing
        'loco.constraints.bits-int-channeling
        'loco.constraints.bools-int-channeling
        'loco.constraints.cardinality
        'loco.constraints.circuit
        'loco.constraints.clauses-int-channeling
        'loco.constraints.count
        'loco.constraints.cumulative
        'loco.constraints.diff-n
        ;'loco.constraints.distance
        'loco.constraints.element
        'loco.constraints.int-value-precede-chain
        'loco.constraints.inverse-channeling
        'loco.constraints.knapsack
        'loco.constraints.lex-chain-less
        'loco.constraints.lex-chain-less-equal
        'loco.constraints.lex-less
        'loco.constraints.lex-less-equal
        'loco.constraints.max
        'loco.constraints.member
        'loco.constraints.min
        'loco.constraints.n-values
        'loco.constraints.not-member
        'loco.constraints.nth
        'loco.constraints.path
        'loco.constraints.scalar
        'loco.constraints.sort
        'loco.constraints.square
        'loco.constraints.sub-circuit
        'loco.constraints.sub-path
        'loco.constraints.table
        'loco.constraints.tree

        'loco.constraints.set.intersection
        'loco.constraints.set.union
        'loco.constraints.set.nb-empty
        'loco.constraints.set.not-empty
        'loco.constraints.set.off-set
        'loco.constraints.set.partition
        'loco.constraints.set.subset-eq
        'loco.constraints.set.sum-elements
        'loco.constraints.set.symetric
        'loco.constraints.set.all-disjoint
        'loco.constraints.set.disjoint
        'loco.constraints.set.set-bools-channeling
        'loco.constraints.set.sets-ints-channeling
        'loco.constraints.set.inverse

        'loco.constraints.logic.logic
        'loco.constraints.automata.regular
        ]
       (map ns-publics)
       (into {})))

(doseq [[sym var] to-inherit]
  (inherit-def "$" sym var))

(def $true [:constraint :true])

(def $false [:constraint :false])

(load "vars")
(load "constraints/sum")

;; -------------------- MDD --------------------
;; mddc(IntVar[] vars, MultivaluedDecisionDiagram MDD)
;; requires building a complex object of ints and tuples
;;http://www.choco-solver.org/apidocs/org/chocosolver/util/objects/graphs/MultivaluedDecisionDiagram.html


;;TODO: implement allDifferentUnderCondition(IntVar[] vars, Condition condition, boolean singleCondition)
;;TODO: figure out how to convert a function into a Condition object
;;http://www.choco-solver.org/apidocs/org/chocosolver/solver/constraints/nary/alldifferent/conditions/Condition.html
;;possibly need to use reify
#_(defn $distinct-under-condidiont
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
#_(defn $key-sort
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
