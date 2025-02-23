;; FIXME: WIP


;; (ns ^:compiler loco.compiler-test
;;   (:require [loco.compiler :as compiler]
;;             [loco.model :as model])
;;   (:use clojure.test loco.model.test loco.constraints)
;;   (:import org.chocosolver.solver.Model))

;; (deftest compiling-constraints-test
;;   (testing "neg"
;;     (constraints-assert
;;      '("TABLE ([CSPLarge({x = {-5..5}, , y = {0..2}, , x*y = {-25..10}, })])"
;;        "ARITHM ([-(x*y) = 0])")
;;      [($in :x -5 5)
;;       ($in :y 0 2)
;;       ;;($= 0 ($neg ($* :x :y)))
;;       ($= 0 ($minus-view ($* :x :y)))])
;;     )

;;   (testing "subtration"
;;     (are [expected input] (= expected
;;                              (->> input model/compile compiler/compile :model .getCstrs (map str)))

;;       '("SUM ([PropXplusYeqZ(y, -(x), y-x)])"
;;         "SUM ([PropXplusYeqZ(x, -(y-x), x-y-x)])"
;;         "ARITHM ([x-y-x = 5])")
;;       [($in :x 0 5)
;;        ($in :y 0 5)
;;        ($= 5 ($- :x ($- :y :x)))]))



;;   (testing "square"
;;     (constraints-assert
;;      '("SQUARE ([y = {0..25} = x = {0..5}^2])")
;;      [($in :x 0 5)
;;       ($in :y 0 (* 5 5))
;;       ($square :y :x)]))




;;   (testing "max"
;;     (constraints-assert
;;      '("MAX ([z = {0..2} = max(x = {0..5}, y = {0..2}, 3 = 3)])")
;;      [($in :x 0 5)
;;       ($in :y 0 2)
;;       ($in :z 0 2)
;;       ($max :z [:x :y 3])]
;;      )

;;     (constraints-assert
;;      '("MAX ([z = [0,1].MAX(x = [0,1],y = [0,1])])")
;;      [($bools :x :y :z)
;;       ($max :z [:x :y])]
;;      "should optimize for bools")

;;     (constraints-assert
;;      '("MAX ([max_x_y = {0..5}.MAX(x = {0..5},y = {0..2})])"
;;        "ARITHM ([prop(z.EQ.max_x_y)])")
;;      [($in :x 0 5)
;;       ($in :y 0 2)
;;       ($in :z 0 2)
;;       ($= :z ($max :x :y))]
;;      )

;;     (constraints-assert
;;      '("SETMAX ([PropNotEmpty(set), PropMaxElement(set, max)])"
;;        "SETMAX ([PropMaxElement(set, max)])")
;;      [($in :max 0 50)
;;       ($set :set [1 2 3 4 5 6 7])
;;       ($max :set :max true)
;;       ($max :set :max false)]
;;      )

;;     (constraints-assert
;;      '("SETMAX ([PropNotEmpty(set-indices), PropMaxElement(set-indices, max)])")
;;      [($in :max 0 50)
;;       ($set :set-indices [1 2 3 4 5 6 7])
;;       ($max :set-indices [9 3 28 1 4 50 6 2 100] 4 :max true)]
;;      )
;;     )

;;   (testing "min"
;;     (constraints-assert
;;      '("MIN ([z = {0..2} = min(x = {0..5}, y = {0..2}, 3 = 3)])")
;;      [($in :x 0 5)
;;       ($in :y 0 2)
;;       ($in :z 0 2)
;;       ($min :z [:x :y 3])]
;;      )

;;     (constraints-assert
;;      '("MIN ([z = [0,1].MIN(x = [0,1],y = [0,1])])")
;;      [($bools :x :y :z)
;;       ($min :z [:x :y])]
;;      "should optimize for bools")

;;     (constraints-assert
;;      '("MIN ([min_x_y = {0..2}.MIN(x = {0..5},y = {0..2})])"
;;        "ARITHM ([prop(z.EQ.min_x_y)])")
;;      [($in :x 0 5)
;;       ($in :y 0 2)
;;       ($in :z 0 2)
;;       ($= :z ($min :x :y))]
;;      )

;;     (constraints-assert
;;      '("SETMIN ([PropNotEmpty(set), PropMinElement(set, min)])"
;;        "SETMIN ([PropMinElement(set, min)])")
;;      [($in :min 0 50)
;;       ($set :set [1 2 3 4 5 6 7])
;;       ($min :set :min true)
;;       ($min :set :min false)]
;;      )

;;     (constraints-assert
;;      '("SETMIN ([PropNotEmpty(set-indices), PropMinElement(set-indices, min)])")
;;      [($in :min 0 50)
;;       ($set :set-indices [1 2 3 4 5 6 7])
;;       ($min :set-indices [9 3 28 1 4 50 6 2 100] 4 :min true)]
;;      )
;;     )

;;   (testing "scalrar"
;;     (constraints-assert
;;      '("TABLE ([CSPLarge({1s = {0..9}, , 10s = {0..9}, , 100s = {0..9}, , 999? = {0..999}, })])")
;;      [($in :1s 0 9)
;;       ($in :10s 0 9)
;;       ($in :100s 0 9)
;;       ($in :999? 0 999)
;;       ($scalar :999? := [:1s :10s :100s] [1 10 100])]
;;      )
;;     )

;;   (testing "all different"
;;     (constraints-assert
;;      '("ALLDIFFERENT ([PropAllDiffInst(a, b, c, 0), PropAllDiffBC(a, b, c, 0), PropAllDiffAdaptative(a, b, c, 0)])")
;;      [($in :a 0 9)
;;       ($in :b 0 9)
;;       ($in :c 0 9)
;;       ($distinct [:a :b :c 0])]
;;      )

;;     (constraints-assert
;;      '("SETALLDIFFERENT ([PropAllDiff(a, b, c), PropAllDiff(a, b, c), PropAtMost1Empty(a, b, c)])")
;;      [($set :a [0 9] [0 9 1 2 3 4 5])
;;       ($set :b [0 9] [0 9 1 2 3 4 5])
;;       ($set :c [0 9] [0 9 1 2 3 4 5])
;;       ($distinct [:a :b :c])]
;;      )

;;     (constraints-assert
;;      '("ALLDIFFERENT ([PropAllDiffInst(a, b), PropAllDiffAC(a, b)])")
;;      [($in :a 0 9)
;;       ($in :b 0 9)
;;       ($distinct [:a :b] :ac)]
;;      )

;;     (constraints-assert
;;      '("ALLDIFFERENT ([PropAllDiffInst(a, b), PropAllDiffBC(a, b)])")
;;      [($in :a 0 9)
;;       ($in :b 0 9)
;;       ($distinct [:a :b] :bc)]
;;      )
;;     )

;;   (testing "all different except 0"
;;     (constraints-assert
;;      '("ALLDIFFERENT ([PropCondAllDiffInst(a, b, c, 0), PropCondAllDiff_AC(a, b, c, 0)])")
;;      [($in :a 0 9)
;;       ($in :b 0 9)
;;       ($in :c 0 9)
;;       ($distinct-except-0 [:a :b :c 0])]
;;      )
;;     )

;;   (testing "circuit"
;;     (constraints-assert
;;      '("CIRCUIT ([PropAllDiffInst(a, b, c), PropAllDiffAC(a, b, c), PropNoSubTour([a = {10..99}, b = {0..9}, c = {100..1000}]), PropCircuit_ArboFiltering(a, b, c), PropCircuit_AntiArboFiltering(a, b, c), PropCircuitSCC(a, b, c)])")
;;      [($in :a 10 99)
;;       ($in :b 0 9)
;;       ($in :c 100 1000)
;;       ($circuit [:a :b :c])]
;;      )

;;     (constraints-assert
;;      '("CIRCUIT ([PropAllDiffInst(a, b, c), PropAllDiffAC(a, b, c), PropNoSubTour([a = {10..99}, b = {0..9}, c = {100..1000}])])")
;;      [($in :a 10 99)
;;       ($in :b 0 9)
;;       ($in :c 100 1000)
;;       ($circuit [:a :b :c] 0 :light)]
;;      )

;;     (constraints-assert
;;      '("CIRCUIT ([PropAllDiffInst(a, b, c), PropAllDiffAC(a, b, c), PropNoSubTour([a = {10..99}, b = {0..9}, c = {100..1000}]), PropCircuit_ArboFiltering(a, b, c), PropCircuit_AntiArboFiltering(a, b, c), PropCircuitSCC(a, b, c)])")
;;      [($in :a 10 99)
;;       ($in :b 0 9)
;;       ($in :c 100 1000)
;;       ($circuit [:a :b :c] 0 :first)]
;;      )

;;     (constraints-assert
;;      '("CIRCUIT ([PropAllDiffInst(a, b, c), PropAllDiffAC(a, b, c), PropNoSubTour([a = {10..99}, b = {0..9}, c = {100..1000}]), PropCircuit_ArboFiltering(a, b, c), PropCircuit_AntiArboFiltering(a, b, c), PropCircuitSCC(a, b, c)])")
;;      [($in :a 10 99)
;;       ($in :b 0 9)
;;       ($in :c 100 1000)
;;       ($circuit [:a :b :c] 0 :rd)]
;;      )
;;     )

;;   (testing "knapsack"
;;     (constraints-assert
;;      '("KNAPSACK ([3.x + 1.y + 2.z - 1.W = 0, 5.x + 6.y + 7.z - 1.V = 0, PropKnapsack(x, y, z, ..., V)])")
;;      [($in :x 0 50)
;;       ($in :y 25 50)
;;       ($in :z 100 150)
;;       ($in :W 0 200)
;;       ($in :V 0 200)
;;       ($knapsack [3 1 2]                ; weights
;;                  [5 6 7]                ; values
;;                  [:x :y :z]             ; occurrences
;;                  :W                     ; total weight
;;                  :V)]))

;;   (testing "member"
;;     (constraints-assert
;;      '("MEMBER ([x in [25,75]])")
;;      [($in :x 0 50)
;;       ($member :x 25 75)])

;;     (constraints-assert
;;      '("MEMBER ([x in [29, 28, 27, 26, 25]])")
;;      [($in :x 0 50)
;;       ($member :x (range 25 30))])

;;     (constraints-assert
;;      '("SETMEMBER ([PropIntEnumMemberSet(set, x)])")
;;      [($in :x 0 50)
;;       ($set :set [1 2 3 4 5 6 7 8])
;;       ($member :x :set)])

;;     (constraints-assert
;;      '("SETMEMBER ([PropIntCstMemberSet(set)])")
;;      [($set :set [1 2 3 4 5 6 7 8])
;;       ($member 4 :set)])

;;     (constraints-assert
;;      '("SETELEMENT ([PropElement(a, b, c, ..., idx_tmp), PropElement(a, b, c, ..., idx_tmp)])")
;;      [($set :a [1 2 3 4 5 6 7 8])
;;       ($set :b [0 1] [0 1 9])
;;       ($set :c [3 4] [1 2 3 4])
;;       ($set :member-of [1 2] [1 2 3 4 5 6])
;;       ($member :member-of [:a :b :c])])
;;     )

;;   (testing "not-member"
;;     (constraints-assert
;;      '("NOTMEMBER ([x outside [25,75]])")
;;      [($in :x 0 50)
;;       ($not-member :x 25 75)])

;;     (constraints-assert
;;      '("NOTMEMBER ([x outside [29, 28, 27, 26, 25]])")
;;      [($in :x 0 50)
;;       ($not-member :x (range 25 30))])

;;     (constraints-assert
;;      '("SETNOTMEMBER ([PropNotMemberIntSet(x, set), PropNotMemberSetInt(set)])")
;;      [($in :x 0 50)
;;       ($set :set [1 2 3 4 5 6 7 8])
;;       ($not-member :x :set)])

;;     (constraints-assert
;;      '("SETNOTMEMBER ([PropIntCstNotMemberSet(set)])")
;;      [($set :set [1 2 3 4 5 6 7 8])
;;       ($not-member 4 :set)])
;;     )

;;   (testing "n-values"
;;     (constraints-assert
;;      '("NVALUES ([PropAtLeastNValues(x, y, z, nvalue), PropAtMostNValues(x, y, z, nvalue), PropAMNV(x, y, z, nvalue)])")
;;      [($in :x 0 5)
;;       ($in :y 0 5)
;;       ($in :z 0 5)
;;       ($in :nvalue 1 3)
;;       ($n-values [:x :y :z] :nvalue)]))

;;   (testing "sort"
;;     (constraints-assert
;;      '("KEYSORT ([PropKeysorting(a, b, c, ..., p_3)])")
;;      [($in :x 0 5)
;;       ($in :y 0 5)
;;       ($in :z 0 5)
;;       ($in :a 0 5)
;;       ($in :b 0 5)
;;       ($in :c 0 5)
;;       ($sort [:a :b :c] [:x :y :z])]))

;;   ;;wtf lawl


;;   (testing "clauses-int-channeling"
;;     ;;TODO: throw as model/compile time
;;     (is
;;       (thrown?
;;        org.chocosolver.solver.exception.SolverException
;;        (->> [($int :int 0 4)
;;              ($bools :a :b :c :d)
;;              ($bools :e :f :g :h)
;;              ($clauses-int-channeling :int [:a :b :c :d] [:e :f :g :h])]
;;             model/compile
;;             compiler/compile))
;;       "should throw when domain of int is larger than bools-list size")

;;     (is
;;      (compile-constraint?
;;       '("CLAUSESINTCHANNELING ([PropClauseChanneling(int, a, b, ..., h)])")
;;       [($int :int 0 3)
;;        ($bools :a :b :c :d)
;;        ($bools :e :f :g :h)
;;        ($clauses-int-channeling :int [:a :b :c :d] [:e :f :g :h])]))
;;     )

;;   (testing "sub-circuit"
;;     (is
;;      (compile-constraint?
;;       '("ARITHM ([sub-length = {1..3} + nLoops = [0,3] = 3])"
;;         "SUBCIRCUIT ([PropAllDiffInst(a, b, c), PropAllDiffAC(a, b, c), PropKLoops(a, b, c, nLoops), PropSubcircuit(a, b, c, sub-length), PropSubcircuitDominatorFilter(a, b, c)])")
;;       [($int :a 0 3)
;;        ($int :b 0 3)
;;        ($int :c 0 3)
;;        ($int :sub-length 1 3)
;;        ($sub-circuit [:a :b :c] :sub-length 0)]))
;;     )

;;   (testing "int-value-precede-chain"
;;     (is
;;      (compile-constraint?
;;       '("INT_VALUE_PRECEDE ([PropIntValuePrecedeChain(a, b, c), PropIntValuePrecedeChain(a, b, c)])")
;;       [($int :a 0 3)
;;        ($int :b 0 3)
;;        ($int :c 0 3)
;;        ($int-value-precede-chain [:a :b :c] [0 1 2])]))

;;     (is
;;      (compile-constraint?
;;       '("INT_VALUE_PRECEDE ([PropIntValuePrecedeChain(a, b, c)])")
;;       [($int :a 0 3)
;;        ($int :b 0 3)
;;        ($int :c 0 3)
;;        ($int-value-precede-chain [:a :b :c] 0 3)]))
;;     )

;;   (testing "lex-less"
;;     (is
;;      (compile-constraint?
;;       '("LEX ([LEX <a = {0..3}, b = {0..3}, c = {0..3}>, <x = {0..3}, y = {0..3}, z = {0..3}>])")
;;       [($int :a 0 3)
;;        ($int :b 0 3)
;;        ($int :c 0 3)
;;        ($int :x 0 3)
;;        ($int :y 0 3)
;;        ($int :z 0 3)
;;        ($lex-less [:a :b :c] [:x :y :z])]))
;;     )

;;   (testing "lex-less-equal"
;;     (is
;;      (compile-constraint?
;;       '("LEX ([LEX <a = {0..3}, b = {0..3}, c = {0..3}>, <x = {0..3}, y = {0..3}, z = {0..3}>])")
;;       [($int :a 0 3)
;;        ($int :b 0 3)
;;        ($int :c 0 3)
;;        ($int :x 0 3)
;;        ($int :y 0 3)
;;        ($int :z 0 3)
;;        ($lex-less-equal [:a :b :c] [:x :y :z])]))
;;     )

;;   (testing "lex-chain-less"
;;     (is
;;      (compile-constraint?
;;       '("LEXCHAIN ([PropLexChain(a, b, c, ..., d)])")
;;       [($int :a 0 3)
;;        ($int :b 0 3)
;;        ($int :c 0 3)
;;        ($int :d 0 3)
;;        ($lex-chain-less [:a :b :c] [:b :c :d])]))

;;     (is
;;      (compile-constraint?
;;       '("LEXCHAIN ([PropLexChain(a, b, c, ..., d)])")
;;       [($int :a 0 3)
;;        ($int :b 0 3)
;;        ($int :c 0 3)
;;        ($int :d 0 3)
;;        ($lex-chain-less [[:a :b :c] [:b :c :d]])]))
;;     )

;;   (testing "lex-chain-less-equal"
;;     (is
;;      (compile-constraint?
;;       '("LEXCHAIN ([PropLexChain(a, b, c, ..., d)])")
;;       [($int :a 0 3)
;;        ($int :b 0 3)
;;        ($int :c 0 3)
;;        ($int :d 0 3)
;;        ($lex-chain-less-equal [:a :b :c] [:b :c :d])]))

;;     (is
;;      (compile-constraint?
;;       '("LEXCHAIN ([PropLexChain(a, b, c, ..., d)])")
;;       [($int :a 0 3)
;;        ($int :b 0 3)
;;        ($int :c 0 3)
;;        ($int :d 0 3)
;;        ($lex-chain-less-equal [[:a :b :c] [:b :c :d]])]))

;;     )

;;   (testing "path"
;;     (is
;;      (compile-constraint?
;;       '("PATH ([prop(start.NEQ.end), PropAllDiffInst(a, b, c, start), PropAllDiffAC(a, b, c, start), PropNoSubTour([a = {0..3}, b = {0..3}, c = {0..3}, start = {0..3}]), PropCircuit_ArboFiltering(a, b, c, start), PropCircuit_AntiArboFiltering(a, b, c, start), PropCircuitSCC(a, b, c, start), PropElementV_fast(cste -- 3, end, a, ..., c)])")
;;       [($int :a 0 3)
;;        ($int :b 0 3)
;;        ($int :c 0 3)
;;        ($int :start 0 3)
;;        ($int :end 0 3)
;;        ($path [:a :b :c] :start :end)]))

;;     (is
;;      (compile-constraint?
;;       '("PATH ([prop(start.NEQ.end), PropAllDiffInst(a, b, c, start), PropAllDiffAC(a, b, c, start), PropNoSubTour([a = {0..3}, b = {0..3}, c = {0..3}, start = {0..3}]), PropCircuit_ArboFiltering(a, b, c, start), PropCircuit_AntiArboFiltering(a, b, c, start), PropCircuitSCC(a, b, c, start), PropElementV_fast(cste -- 4, end, a, ..., c)])")
;;       [($int :a 0 3)
;;        ($int :b 0 3)
;;        ($int :c 0 3)
;;        ($int :start 0 3)
;;        ($int :end 0 3)
;;        ($path [:a :b :c] :start :end 1)]))

;;     )

;;   (testing "sub-path"
;;     (is
;;      (compile-constraint?
;;       '("ARITHM ([(size = {0..3} + 1) = [1,4] + nLoops = [0,4] = 4])"
;;         "SUBPATH ([start <= 2, PropAllDiffInst(a, b, c, start), PropAllDiffAC(a, b, c, start), PropKLoops(a, b, c, ..., nLoops), PropSubcircuit(a, b, c, ..., (size+1)), PropSubcircuitDominatorFilter(a, b, c, start), PropElementV_fast(cste -- 3, end, a, ..., c)])")
;;       [($int :a 0 3)
;;        ($int :b 0 3)
;;        ($int :c 0 3)
;;        ($int :start 0 3)
;;        ($int :end 0 3)
;;        ($int :size 0 3)
;;        ($sub-path [:a :b :c] :start :end :size)]))

;;     (is
;;      (compile-constraint?
;;       '("ARITHM ([(size = {0..3} + 1) = [1,4] + nLoops = [0,4] = 4])"
;;         "SUBPATH ([start <= 3, PropAllDiffInst(a, b, c, start), PropAllDiffAC(a, b, c, start), PropKLoops(a, b, c, ..., nLoops), PropSubcircuit(a, b, c, ..., (size+1)), PropSubcircuitDominatorFilter(a, b, c, start), PropElementV_fast(cste -- 4, end, a, ..., c)])")
;;       [($int :a 0 3)
;;        ($int :b 0 3)
;;        ($int :c 0 3)
;;        ($int :start 0 3)
;;        ($int :end 0 3)
;;        ($int :size 0 3)
;;        ($sub-path [:a :b :c] :start :end :size 1)]))

;;     )

;;   (testing "inverse-channeling"
;;     (is
;;      (compile-constraint?
;;       '("INVERSECHANNELING ([PropAllDiffInst(a, b, c), PropAllDiffBC(a, b, c), PropAllDiffAdaptative(a, b, c), PropAllDiffInst(x, y, z), PropAllDiffBC(x, y, z), PropAllDiffAdaptative(x, y, z), Inverse_AC({a = {0..3}...}{x = {0..3}...})])")
;;       [($int :a 0 3)
;;        ($int :b 0 3)
;;        ($int :c 0 3)
;;        ($int :x 0 3)
;;        ($int :y 0 3)
;;        ($int :z 0 3)
;;        ($inverse-channeling [:a :b :c] [:x :y :z])]))

;;     (is
;;      (compile-constraint?
;;       '("INVERSECHANNELING ([PropAllDiffInst(a, b, c), PropAllDiffBC(a, b, c), PropAllDiffAdaptative(a, b, c), PropAllDiffInst(x, y, z), PropAllDiffBC(x, y, z), PropAllDiffAdaptative(x, y, z), Inverse_AC({a = {0..3}...}{x = {0..3}...})])")
;;       [($int :a 0 3)
;;        ($int :b 0 3)
;;        ($int :c 0 3)
;;        ($int :x 0 3)
;;        ($int :y 0 3)
;;        ($int :z 0 3)
;;        ($inverse-channeling [:a :b :c] 1 [:x :y :z] 1)]))
;;     )

;;   (testing "tree"
;;     (is
;;      (compile-constraint?
;;       '("TREE ([PropAntiArborescences(a, b, c), PropKLoops(a, b, c, nb-trees)])")
;;       [($int :a 0 3)
;;        ($int :b 0 3)
;;        ($int :c 0 3)
;;        ($int :nb-trees 0 100)
;;        ($tree [:a :b :c] :nb-trees)]))

;;     (is
;;      (compile-constraint?
;;       '("TREE ([PropAntiArborescences(a, b, c, d), PropKLoops(a, b, c, ..., nb-trees)])")
;;       [($int :a 0 3)
;;        ($int :b 0 3)
;;        ($int :c 0 3)
;;        ($int :d 0 3)
;;        ($int :nb-trees 0 100)
;;        ($tree [:a :b :c :d] :nb-trees 1)]))
;;     )



;;   (testing "table"
;;     (is
;;      (compile-constraint?
;;       '("TABLE ([CSPLarge({a = {0..20}, })])")
;;       [($int :a 0 20)
;;        ($tuples :mask-0-9 [[1][2][3][4][5][6][7][8][9][0]])
;;        ($table [:a] :mask-0-9)
;;        ]
;;       ))

;;     (is
;;      (compile-constraint?
;;       '("TABLE ([CSPLarge({a = {0..20}, })])")
;;       [($int :a 0 20)
;;        ($tuples :mask-0-9 [[1][2][3][4][5][6][7][8][9][0]] false)
;;        ($table [:a] :mask-0-9)
;;        ]
;;       ))

;;     (is
;;      (compile-constraint?
;;       '("TABLE ([PropCompactTable(a)])")
;;       [($int :a 0 20)
;;        ($tuples :mask-0-9 [[1][2][3][4][5][6][7][8][9][0]])
;;        ($table [:a] :mask-0-9 :CT+)
;;        ]
;;       ))

;;     (is
;;      (compile-constraint?
;;       '("TABLE ([PropBinAC3bitrm(a, b)])")
;;       [($int :a 0 20)
;;        ($int :b 0 20)
;;        ($tuples :mask [[1 9][2 8][3 7][4 6][5 5][6 4][7 3][8 2][9 1][0 0]])
;;        ($table :a :b :mask)
;;        ]
;;       ))

;;     (is
;;      (compile-constraint?
;;       '("TABLE ([PropBinAC3rm(a, b)])")
;;       [($int :a 0 20)
;;        ($int :b 0 20)
;;        ($tuples :mask [[1 9][2 8][3 7][4 6][5 5][6 4][7 3][8 2][9 1][0 0]])
;;        ($table :a :b :mask :AC3rm)
;;        ]
;;       ))
;;     )
;;   )

;; (deftest logic-compile-test
;;   (testing "and"
;;     (constraints-assert
;;      '("SUM ([b3 + b2 + b1 = IV_1 + 0])" "ARITHM ([IV_1 = 3])")
;;      [($bool :b1)
;;       ($bool :b2)
;;       ($bool :b3)
;;       ($and :b1 :b2 :b3)]
;;      "should handle list of booleans")

;;     (constraints-assert
;;      '("REIF_1 = [0,1]=>TRUE ([true]), !REIF_1 = [0,1]=>OPPOSITE ([PropOpposite(cste -- 1)])"
;;        "REIF_2 = [0,1]=>FALSE ([false]), !REIF_2 = [0,1]=>OPPOSITE ([PropOpposite(cste -- 0)])"
;;        "SUM ([PropXplusYeqZ(REIF_1, REIF_2, IV_3)])"
;;        "ARITHM ([IV_3 = 2])")
;;      [($and ($true) ($false))]
;;      )
;;     )

;;   (testing "or"
;;     (constraints-assert
;;      '("SUM ([b3 + b2 + b1 = IV_1 + 0])" "ARITHM ([IV_1 >= 1])")
;;      [($bool :b1)
;;       ($bool :b2)
;;       ($bool :b3)
;;       ($or :b1 :b2 :b3)]
;;      "should handle list of booleans")

;;     (constraints-assert
;;      '("REIF_1 = [0,1]=>TRUE ([true]), !REIF_1 = [0,1]=>OPPOSITE ([PropOpposite(cste -- 1)])"
;;        "REIF_2 = [0,1]=>FALSE ([false]), !REIF_2 = [0,1]=>OPPOSITE ([PropOpposite(cste -- 0)])"
;;        "SUM ([PropXplusYeqZ(REIF_1, REIF_2, IV_3)])"
;;        "ARITHM ([IV_3 >= 1])")
;;      [($or ($true) ($false))]
;;      )
;;     )

;;   (testing "when"
;;     (constraints-assert
;;      '("REIF_1 = [0,1]=>FALSE ([false]), !REIF_1 = [0,1]=>OPPOSITE ([PropOpposite(cste -- 0)])"
;;        "ARITHM ([REIF_1 = 0])")
;;      [($when ($false) ($false))])

;;     (constraints-assert
;;      '("ARITHM ([when = 0])")
;;      [($bool :when)
;;       ($when :when ($false))])

;;     (constraints-assert
;;      '("REIF_1 = [0,1]=>TRUE ([true]), !REIF_1 = [0,1]=>OPPOSITE ([PropOpposite(cste -- 1)])"
;;        "ARITHM ([prop(REIF_1.GEQ.when)])")
;;      [($bool :when)
;;       ($when :when ($true))])
;;     )

;;   (testing "if-then-else"
;;     (constraints-assert
;;      '("REIF_1 = [0,1]=>TRUE ([true]), !REIF_1 = [0,1]=>OPPOSITE ([PropOpposite(cste -- 1)])"
;;        "REIF_2 = [0,1]=>TRUE ([true]), !REIF_2 = [0,1]=>OPPOSITE ([PropOpposite(cste -- 1)])"
;;        "ARITHM ([prop(REIF_2.GEQ.REIF_1)])"
;;        "ARITHM ([not(REIF_1) = 0])")
;;      [($if ($true) ($true) ($false))])

;;     (constraints-assert
;;      '("REIF_1 = [0,1]=>TRUE ([true]), !REIF_1 = [0,1]=>OPPOSITE ([PropOpposite(cste -- 1)])"
;;        "ARITHM ([prop(REIF_1.GEQ.if)])"
;;        "ARITHM ([not(if) = 0])")
;;      [($bool :if)
;;       ($if :if ($true) ($false))])
;;     )

;;   (testing "cond"
;;     (constraints-assert
;;      '()
;;      [($cond
;;        ($false) ($true)
;;        ($false) ($false)
;;        ($true) ($true)
;;        ($false) ($true)
;;        :else ($true))])
;;     )

;;   (testing "not"
;;     (constraints-assert
;;      '("OPPOSITE ([PropOpposite(cste -- 0)])")
;;      [($not ($false))])

;;     (constraints-assert
;;      '("TRUE ([true])")
;;      [($not ($not ($true)))])

;;     (constraints-assert
;;      '("ARITHM ([prop(b.GEQ.a)])"
;;        "ARITHM ([prop(a.NEQ.b)])"
;;        "SUM ([a + b + c != 5])")
;;      [($in :a 0 5)
;;       ($in :b 0 5)
;;       ($in :c 0 1)
;;       ($not ($> :a :b))
;;       ($not ($= :a :b))
;;       ($not ($sum 5 := [:a :b :c]))]))

;;   (testing "true false"
;;     (constraints-assert
;;      '("TRUE ([true])")
;;      [($true)])

;;     (constraints-assert
;;      '("FALSE ([false])")
;;      [($false)]))
;;   )

;; (deftest reify-compile-test
;;   (constraints-assert
;;    '("SUM ([PropXplusYeqZ(b, c, IV_1)])"
;;      "a = [0,1]=>ARITHM ([IV_1 = 2]), !a = [0,1]=>ARITHM ([IV_1 =/= 2])")
;;    [($bool :b)
;;     ($bool :c)
;;     ($reify :a ($and :b :c))])

;;   (constraints-assert
;;    '("a = [0,1]=>ARITHM ([prop(c.EQ.b+2)]), !a = [0,1]=>ARITHM ([prop(c.NEQ.b+2)])"
;;      "ARITHM ([prop(b.EQ.b+2+-2)])")
;;    [($in :b 0 2)
;;     ($in :c 0 2)
;;     ($reify :a ($= :c ($+ :b 2)))])
;;   )

;; (deftest set-constraints-compile-test
;;   (testing "intersection"
;;     (constraints-assert
;;      '("SETINTERSECTION ([PropIntersection(a, b, c, intersection)])")
;;      [($set :intersection [0 1 2 3] [0 1 2 3 4 5 6 7 8 9])
;;       ($set :a [0] [0 9 8])
;;       ($set :b [1] [1 7 6])
;;       ($set :c [2] [2 5 4])
;;       ($intersection :intersection [:a :b :c])]
;;      )
;;     )

;;   (testing "union"
;;     (constraints-assert
;;      '("SETUNION ([PropUnion(a, b, result), PropUnion(a, b, result)])")
;;      [($set :result [0 1] [0 1 9 8 7 6])
;;       ($set :a [0] [0 9 8])
;;       ($set :b [1] [1 7 6])
;;       ($union :result [:a :b])]
;;      )

;;     (constraints-assert
;;      '("SETINTVALUESUNION ([PropSetIntValuesUnion(a, b, result), PropSetIntValuesUnion(a, b, result)])")
;;      [($set :result [] [0 1 9 8 7 6])
;;       ($int :a [0 9 8])
;;       ($int :b [1 7 6])
;;       ($union :result [:a :b])]
;;      "should work with IntVar[] variant")
;;     )

;;   (testing "nb-empty"
;;     (constraints-assert
;;      '("SETNBEMPTY ([PropNbEmpty(a, b, c, empty)])")
;;      [($set :a [] [0 9 8])
;;       ($set :b [] [1 7 6])
;;       ($set :c [] [0 9 3])
;;       ($int :empty 0 3)
;;       ($nb-empty :empty [:a :b :c])])

;;     (constraints-assert
;;      '("SETNBEMPTY ([PropNbEmpty(a, b, c, cste -- 3)])")
;;      [($set :a [] [0 9 8])
;;       ($set :b [] [1 7 6])
;;       ($set :c [] [0 9 3])
;;       ($count-empty 3 [:a :b :c])])
;;     )

;;   (testing "not-empty"
;;     (is (compile-constraint?
;;          '("SETNOTEMPTY ([PropNotEmpty(a)])")
;;          [($set :a [] [0 1 2 3])
;;           ($not-empty :a)])))

;;   (testing "off-set"
;;     (is (compile-constraint?
;;          '("SETOFFSET ([PropOffSet(a, b)])")
;;          [($set :a [] [0 1 2 3])
;;           ($set :b [] [2 3 4 5 6])
;;           ($off-set :a :b 1)])))

;;   (testing "partition"
;;     (is
;;      (compile-constraint?
;;       '("SETPARTITION ([PropAllDisjoint(a, b, c, d), PropUnion(a, b, c, ..., universe), PropUnion(a, b, c, ..., universe)])")
;;       [
;;        ($set :a [0] [0 1])
;;        ($set :b [1] [1 2])
;;        ($set :c [2] [2 3])
;;        ($set :d [3] [3 4])
;;        ($set :universe [0 1 2 3 4])
;;        ($partition :universe [:a :b :c :d])
;;        ]))
;;     )

;;   (testing "subset-equal"
;;     (is
;;      (compile-constraint?
;;       '("SETSUBSETEQ ([PropSubsetEq(a, b)])")
;;       [
;;        ($set :a [0 1 2] [0 1 2 3 4 5])
;;        ($set :b [1] [1 2])
;;        ($subset-equal :a :b)
;;        ]))

;;     (is
;;      (compile-constraint?
;;       '("SETSUBSETEQ ([PropSubsetEq(a, b)])")
;;       [
;;        ($set :a [0 1 2] [0 1 2 3 4 5])
;;        ($set :b [1] [1 2])
;;        ($subset-equal [:a :b])
;;        ]))
;;     )

;;   (testing "sum-elements"
;;     (is
;;      (compile-constraint?
;;       '("SETSUM ([PropSumOfElements(indices, sum)])")
;;       [
;;        ($set :indices [0 1 2] [0 1 2 3 4 5])
;;        ($int :sum 0 1000)
;;        ($sum-elements :sum :indices [10 200 3000 40000 50000 600000])
;;        ]))

;;     (is
;;      (compile-constraint?
;;       '("SETSUM ([PropSumOfElements(indices, sum)])")
;;       [
;;        ($set :indices [0 1 2] [0 1 2 3 4 5])
;;        ($int :sum 0 1000)
;;        ($sum-elements :sum :indices [10 200 3000 40000 50000 600000] 1)
;;        ]))
;;     )

;;   (testing "symetric"
;;     (is
;;      (compile-constraint?
;;       '("SETSYMMETRIC ([PropSymmetric(a, b, c, d)])")
;;       [
;;        ($set :a [0] [0 1])
;;        ($set :b [1] [1 2])
;;        ($set :c [2] [2 3])
;;        ($set :d [3] [3 4])
;;        ($symetric :a :b :c :d)
;;        ]))

;;     (is
;;      (compile-constraint?
;;       '("SETSYMMETRIC ([PropSymmetric(a, b, c, d)])")
;;       [
;;        ($set :a [0] [0 1])
;;        ($set :b [1] [1 2])
;;        ($set :c [2] [2 3])
;;        ($set :d [3] [3 4])
;;        ($symetric [:a :b :c :d])
;;        ]))

;;     (is
;;      (compile-constraint?
;;       '("SETSYMMETRIC ([PropSymmetric(a, b, c, d)])")
;;       [
;;        ($set :a [0] [0 1])
;;        ($set :b [1] [1 2])
;;        ($set :c [2] [2 3])
;;        ($set :d [3] [3 4])
;;        ($symetric [:a :b :c :d] 1)
;;        ]))
;;     )

;;   (testing "all-disjoint"
;;     (is
;;      (compile-constraint?
;;       '("SETALLDISJOINT ([PropAllDisjoint(a, b, c, d)])")
;;       [
;;        ($set :a [0] [0 1])
;;        ($set :b [1] [1 2])
;;        ($set :c [2] [2 3])
;;        ($set :d [3] [3 4])
;;        ($all-disjoint [:a :b :c :d])
;;        ]))

;;     (is
;;      (compile-constraint?
;;       '("SETALLDISJOINT ([PropAllDisjoint(a, b, c, d)])")
;;       [
;;        ($set :a [0] [0 1])
;;        ($set :b [1] [1 2])
;;        ($set :c [2] [2 3])
;;        ($set :d [3] [3 4])
;;        ($all-disjoint :a :b :c :d)
;;        ]))
;;     )

;;   (testing "disjoint"
;;     (is
;;      (compile-constraint?
;;       '("SETALLDISJOINT ([PropAllDisjoint(a, b)])")
;;       [
;;        ($set :a [0] [0 1])
;;        ($set :b [1] [1 2])
;;        ($disjoint [:a :b])
;;        ]))

;;     (is
;;      (thrown?
;;       AssertionError
;;       ($disjoint [:a :b :b])))

;;     (is
;;      (compile-constraint?
;;       '("SETALLDISJOINT ([PropAllDisjoint(a, b)])")
;;       [
;;        ($set :a [0] [0 1])
;;        ($set :b [1] [1 2])
;;        ($disjoint :a :b)
;;        ]))
;;     )

;;   (testing "set-bools-channeling"
;;     (is
;;      (compile-constraint?
;;       '("SETBOOLCHANNELING ([PropBoolChannel(a, b, c, ..., set)])")
;;       [($bools :a :b :c :d)
;;        ($set :set [] [0 1 2 3])
;;        ($set-bools-channeling :set [:a :b :c :d])]))

;;     (is
;;      (compile-constraint?
;;       ' ("SETBOOLCHANNELING ([PropBoolChannel(a, b, c, ..., set)])")
;;       [($bools :a :b :c :d)
;;        ($set :set [] [0 1 2 3])
;;        ($set-bools-channeling :set [:a :b :c :d] 1)]))
;;     )

;;   (testing "sets-ints-channeling"
;;     (is
;;      (compile-constraint?
;;       '("SETINTCHANNELING ([PropIntChannel(s1, s2, s3, ..., i3), PropIntChannel(s1, s2, s3, ..., i3), PropAllDisjoint(s1, s2, s3)])")
;;       [($int :i1 [0 3])
;;        ($int :i2 [0 3])
;;        ($int :i3 [0 3])
;;        ($set :s1 [] [0 1 2 3])
;;        ($set :s2 [] [0 1 2 3])
;;        ($set :s3 [] [0 1 2 3])
;;        ($sets-ints-channeling [:s1 :s2 :s3] [:i1 :i2 :i3])]))

;;     (is
;;      (compile-constraint?
;;       '("SETINTCHANNELING ([PropIntChannel(s1, s2, s3, ..., i3), PropIntChannel(s1, s2, s3, ..., i3), PropAllDisjoint(s1, s2, s3)])")
;;       [($int :i1 [0 3])
;;        ($int :i2 [0 3])
;;        ($int :i3 [0 3])
;;        ($set :s1 [] [0 1 2 3])
;;        ($set :s2 [] [0 1 2 3])
;;        ($set :s3 [] [0 1 2 3])
;;        ($sets-ints-channeling [:s1 :s2 :s3] 1 [:i1 :i2 :i3] 1)]))

;;     )
;;   )
