(ns ^:compiler loco.compiler-test
  (:require [loco.compiler :as compiler]
            [loco.model :as model])
  (:use clojure.test
        loco.model.test
        loco.constraints)
  (:import org.chocosolver.solver.Model))

(deftest compiling-vars-test

  (testing "consts vars"
    (vars-assert
     '(["7" 7 7 true "7 = 7"]
       ["a" 7 7 true "a = 7"]
       ["b" 4 4 true "b = 4"])
     [($const :7 7)
      ($const :a 7)
      ($const :b 4)]))

  (testing "boolVars"
    (vars-assert
     '(["bool" 0 1 true "bool = [0,1]"])
     [($in :bool 0 1)])

    (vars-assert
     '(["bool" 0 1 true "bool = [0,1]"])
     [($bool :bool)]))

  (testing "intVars"
    (vars-assert
     '(["7" 7 7 true "7 = 7"])
     [($in :7 7 7)])

    (vars-assert
     '(["9" 1 13 true "9 = {1..3,5,8,13}"])
     [($in :9 [1 2 3 5 8 13])])

    (vars-assert
     '(["7" 7 7 true "7 = 7"]
       ["8" -1000 1000 true "8 = {-1000..1000}"]
       ["9" 1 3 false "9 = [1,3]"])
     [($in :7 7 7)
      ($in :8 -1000 1000)
      ($in :9 1 3 :bounded)]))

  (testing "neg"
    (vars-assert
     '(["i" 0 5 true "i = {0..5}"]
       ["-(i)" -5 0 true "-(i = {0..5}) = [-5,0]"])
     [($in :i 0 5)
      ($neg :-i :i)
      ]))
  )

(deftest compiling-constraints-test
  (testing "sum"
    (constraints-assert
     '("SUM ([z + y + x = 1])")
     [($bool :x)
      ($bool :y)
      ($bool :z)
      ($sum 1 := [:x :y :z])]
     "should handle list of booleans")

    (constraints-assert
     '("SUM ([z + y + x = 2])")
     [($in :x 1 2)
      ($in :y 1 2)
      ($in :z 1 2)
      ($sum 2 := [:x :y :z])])

    (constraints-assert
     '("SUM ([z + y + x = 2])")
     [($in :x 1 2)
      ($bool :y)
      ($in :z 1 2)
      ($sum 2 := [:x :y :z])])

    (constraints-assert
     '("SETSUM ([PropSumOfElements(set, sum)])")
     [($in :sum 0 10)
      ($set :set [0 1 2] [0 1 2 3 4 5 6 7])
      ($sum :sum :set)])

    (constraints-assert
     '("SETSUM ([PropSumOfElements(set, sum)])")
     [($in :sum 0 10)
      ($set :set [0 1 2] [0 1 2 3 4 5 6 7])
      ($sum :set :sum)]
     "should be able to reverse sum/set vars for $sum"
     )

    )

  (testing "arithm"
    (constraints-assert
     '("DIVISION ([PropDivXYZ(x, cste -- 10, IV_1, ..., IV_1)])"
       "ARITHM ([prop(y.EQ.IV_1)])")
     [($in :x 0 100)
      ($in :y 0 10)
      ($arithm :y := :x :/ 10)])

    (constraints-assert
     '("ARITHM ([prop(y.EQ.x)])")
     [($in :x 0 100)
      ($in :y 0 10)
      ($arithm :y := :x)])
    )

  (testing "times"
    (constraints-assert
     '("TABLE ([CSPLarge({x = {0..100}, , y = {0..10}, , z = {0..5}, })])")
     [($in :x 0 100)
      ($in :y 0 10)
      ($in :z 0 5)
      ($times :x :y :z)]))

  (testing "mod"
    (constraints-assert
     '("ABSOLUTE ([|T1_1| = [0,100] = |T1_1 = [-100,100]|])"
       "DIVISION ([PropDivXYZ(x, y, T1_1, ..., |T1_1|)])"
       "TIMES ([PropTimesNaive(T1_1, y, T2_2)])"
       "SUM ([PropXplusYeqZ(z, T2_2, x)])")
     [($in :x 0 100)
      ($in :y 0 10)
      ($in :z 0 5)
      ($mod :x :y :z)]))

  (testing "abs"
    (constraints-assert
     '("ABSOLUTE ([y = {0..10} = |z = {-5..0}|])")
     [($in :y 0 10)
      ($in :z -5 0)
      ($abs :y :z)])

    (constraints-assert
     '("ABSOLUTE ([|x| = {0..5} = |x = {-5..5}|])"
      "ARITHM ([|x| = 2])")
     [($in :x -5 5)
      ($= ($abs :x) 2)])
    )

  (testing "neg"
    (constraints-assert
     '("TABLE ([CSPLarge({x = {-5..5}, , y = {0..2}, , x*y = {-25..10}, })])"
       "ARITHM ([-(x*y) = 0])")
     [($in :x -5 5)
      ($in :y 0 2)
      ($= 0 ($neg ($* :x :y)))])
    )

  (testing "subtration"
    (constraints-assert
     '("SUM ([PropXplusYeqZ(y, -(x), y-x)])"
       "SUM ([PropXplusYeqZ(x, -(y-x), x-y-x)])"
       "ARITHM ([x-y-x = 5])")
     [($in :x 0 5)
      ($in :y 0 5)
      ($= 5 ($- :x ($- :y :x)))]))

  (testing "div"
    (constraints-assert
     '("DIVISION ([PropDivXYZ(x, y, 0, ..., cste -- 0)])")
     [($in :x 5 5)
      ($in :y 0 2)
      ($div :x :y 0)]))

  (testing "square"
    (constraints-assert
     '("SQUARE ([y = {0..25} = x = {0..5}^2])")
     [($in :x 0 5)
      ($in :y 0 (* 5 5))
      ($square :y :x)]))

  (testing "all-equal"
    (constraints-assert
     '("ATMOSTNVALUES ([PropAtMostNValues(x, y, 1, cste -- 1)])")
     [($in :x 0 5)
      ($in :y 0 2)
      ($= :x :y 1)]
     )

    (constraints-assert
     '("ATMOSTNVALUES ([PropAtMostNValues(x, y, cste -- 1)])")
     [($in :x 0 5)
      ($in :y 0 2)
      ($all-equal [:x :y])]
     )
    )

  (testing "not-all-equal"
    (constraints-assert
     '("ATLEASTNVALUES ([PropAtLeastNValues(x, y, cste -- 2)])")
     [($in :x 0 5)
      ($in :y 0 2)
      ($not-all-equal [:x :y])]
     )

    (constraints-assert
     '("ATLEASTNVALUES ([PropAtLeastNValues(x, y, 1, cste -- 2)])")
     [($in :x 0 5)
      ($in :y 0 2)
      ($!= :x :y 1)]
     )
    )

  (testing "max"
    (constraints-assert
     '("MAX ([z = {0..2} = max(x = {0..5}, y = {0..2}, 3 = 3)])")
     [($in :x 0 5)
      ($in :y 0 2)
      ($in :z 0 2)
      ($max :z [:x :y 3])]
     )

    (constraints-assert
     '("MAX ([z = [0,1].MAX(x = [0,1],y = [0,1])])")
     [($bools :x :y :z)
      ($max :z [:x :y])]
     "should optimize for bools")

    (constraints-assert
     '("MAX ([max_x_y = {0..5}.MAX(x = {0..5},y = {0..2})])"
       "ARITHM ([prop(z.EQ.max_x_y)])")
     [($in :x 0 5)
      ($in :y 0 2)
      ($in :z 0 2)
      ($= :z ($max :x :y))]
     )

    (constraints-assert
     '("SETMAX ([PropNotEmpty(set), PropMaxElement(set, max)])"
       "SETMAX ([PropMaxElement(set, max)])")
     [($in :max 0 50)
      ($set :set [1 2 3 4 5 6 7])
      ($max :set :max true)
      ($max :set :max false)]
     )

    (constraints-assert
     '("SETMAX ([PropNotEmpty(set-indices), PropMaxElement(set-indices, max)])")
     [($in :max 0 50)
      ($set :set-indices [1 2 3 4 5 6 7])
      ($max :set-indices [9 3 28 1 4 50 6 2 100] 4 :max true)]
     )
    )

  (testing "min"
    (constraints-assert
     '("MIN ([z = {0..2} = min(x = {0..5}, y = {0..2}, 3 = 3)])")
     [($in :x 0 5)
      ($in :y 0 2)
      ($in :z 0 2)
      ($min :z [:x :y 3])]
     )

    (constraints-assert
     '("MIN ([z = [0,1].MIN(x = [0,1],y = [0,1])])")
     [($bools :x :y :z)
      ($min :z [:x :y])]
     "should optimize for bools")

    (constraints-assert
     '("MIN ([min_x_y = {0..2}.MIN(x = {0..5},y = {0..2})])"
       "ARITHM ([prop(z.EQ.min_x_y)])")
     [($in :x 0 5)
      ($in :y 0 2)
      ($in :z 0 2)
      ($= :z ($min :x :y))]
     )

    (constraints-assert
     '("SETMIN ([PropNotEmpty(set), PropMinElement(set, min)])"
       "SETMIN ([PropMinElement(set, min)])")
     [($in :min 0 50)
      ($set :set [1 2 3 4 5 6 7])
      ($min :set :min true)
      ($min :set :min false)]
     )

    (constraints-assert
     '("SETMIN ([PropNotEmpty(set-indices), PropMinElement(set-indices, min)])")
     [($in :min 0 50)
      ($set :set-indices [1 2 3 4 5 6 7])
      ($min :set-indices [9 3 28 1 4 50 6 2 100] 4 :min true)]
     )
    )

  (testing "scalrar"
    (constraints-assert
     '("TABLE ([CSPLarge({1s = {0..9}, , 10s = {0..9}, , 100s = {0..9}, , 999? = {0..999}, })])")
     [($in :1s 0 9)
      ($in :10s 0 9)
      ($in :100s 0 9)
      ($in :999? 0 999)
      ($scalar :999? := [:1s :10s :100s] [1 10 100])]
     )
    )

  (testing "element"
    (constraints-assert
     '("ELEMENT ([element(array-val = {1..5} =  <1, 2, 3, 4, 5> [index = {0..2}])])")
     [($in :index 0 2)
      ($in :array-val 1 5)
      ($element :array-val [1 2 3 4 5] :index)]
     )

    (constraints-assert
     '("ELEMENT ([PropElementV_fast(array-val, index, a, ..., c)])")
     [($in :a 10 99)
      ($in :b 0 9)
      ($in :c 100 1000)
      ($in :index 0 2)
      ($in :array-val 100 1000)
      ($element :array-val [:a :b :c] :index 2)]
     )

    (constraints-assert
     '("ELEMENT ([PropElementV_fast($nth_:a_:2_:3_:4_:5_:at_:index_:offset_0, index, a, ..., 5)])"
       "ARITHM ([$nth_:a_:2_:3_:4_:5_:at_:index_:offset_0 = 4])")
     [($in :a 100 200)
      ($in :index 0 5)
      ($= 4 ($nth [:a 2 3 4 5] :index))]
     )
    )

  (testing "all different"
    (constraints-assert
     '("ALLDIFFERENT ([PropAllDiffInst(a, b, c, 0), PropAllDiffBC(a, b, c, 0), PropAllDiffAdaptative(a, b, c, 0)])")
     [($in :a 0 9)
      ($in :b 0 9)
      ($in :c 0 9)
      ($distinct [:a :b :c 0])]
     )
    )

  (testing "all different except 0"
    (constraints-assert
     '("ALLDIFFERENT ([PropCondAllDiffInst(a, b, c, 0), PropCondAllDiff_AC(a, b, c, 0)])")
     [($in :a 0 9)
      ($in :b 0 9)
      ($in :c 0 9)
      ($distinct-except-0 [:a :b :c 0])]
     )
    )

  (testing "circuit"
    (constraints-assert
     '("CIRCUIT ([PropAllDiffInst(a, b, c), PropAllDiffAC(a, b, c), PropNoSubTour([a = {10..99}, b = {0..9}, c = {100..1000}]), PropCircuit_ArboFiltering(a, b, c), PropCircuit_AntiArboFiltering(a, b, c), PropCircuitSCC(a, b, c)])")
     [($in :a 10 99)
      ($in :b 0 9)
      ($in :c 100 1000)
      ($circuit [:a :b :c])]
     )
    )

  (testing "cardinality"
    (constraints-assert
     '("GCC ([PropFastGCC_(a, b, ones, twos, cste -- 0)])")
     [($in :a 2 3)
      ($in :b 1 3)
      ($cardinality [:a :b] {1 :ones, 2 :twos} :closed)]
     )
    )

  (testing "knapsack"
    (constraints-assert
     '("KNAPSACK ([3.x + 1.y + 2.z - 1.W = 0, 5.x + 6.y + 7.z - 1.V = 0, PropKnapsack(x, y, z, ..., V)])")
     [($in :x 0 50)
      ($in :y 25 50)
      ($in :z 100 150)
      ($in :W 0 200)
      ($in :V 0 200)
      ($knapsack [3 1 2]                ; weights
                 [5 6 7]                ; values
                 [:x :y :z]             ; occurrences
                 :W                     ; total weight
                 :V)]))

  (testing "member"
    (constraints-assert
     '("MEMBER ([x in [25,75]])")
     [($in :x 0 50)
      ($member :x 25 75)])

    (constraints-assert
     '("MEMBER ([x in [29, 28, 27, 26, 25]])")
     [($in :x 0 50)
      ($member :x (range 25 30))])

    (constraints-assert
     '("SETMEMBER ([PropIntEnumMemberSet(set, x)])")
     [($in :x 0 50)
      ($set :set [1 2 3 4 5 6 7 8])
      ($member :x :set)])

    (constraints-assert
     '("SETMEMBER ([PropIntCstMemberSet(set)])")
     [($set :set [1 2 3 4 5 6 7 8])
      ($member 4 :set)])
    )

  (testing "not-member"
    (constraints-assert
     '("NOTMEMBER ([x outside [25,75]])")
     [($in :x 0 50)
      ($not-member :x 25 75)])

    (constraints-assert
     '("NOTMEMBER ([x outside [29, 28, 27, 26, 25]])")
     [($in :x 0 50)
      ($not-member :x (range 25 30))])

    (constraints-assert
     '("SETNOTMEMBER ([PropNotMemberIntSet(x, set), PropNotMemberSetInt(set)])")
     [($in :x 0 50)
      ($set :set [1 2 3 4 5 6 7 8])
      ($not-member :x :set)])

    (constraints-assert
     '("SETNOTMEMBER ([PropIntCstNotMemberSet(set)])")
     [($set :set [1 2 3 4 5 6 7 8])
      ($not-member 4 :set)])
    )

  (testing "n-values"
    (constraints-assert
     '("NVALUES ([PropAtLeastNValues(x, y, z, nvalue), PropAtMostNValues(x, y, z, nvalue), PropAMNV(x, y, z, nvalue)])")
     [($in :x 0 5)
      ($in :y 0 5)
      ($in :z 0 5)
      ($in :nvalue 1 3)
      ($n-values [:x :y :z] :nvalue)]))

  (testing "sort"
    (constraints-assert
     '("KEYSORT ([PropKeysorting(a, b, c, ..., p_3)])")
     [($in :x 0 5)
      ($in :y 0 5)
      ($in :z 0 5)
      ($in :a 0 5)
      ($in :b 0 5)
      ($in :c 0 5)
      ($sort [:a :b :c] [:x :y :z])]))


  (testing "count"
    (constraints-assert
     '("COUNT ([PropFastCount_(a, b, c, limit=limit, value=3)])"
       "COUNT ([PropCountVar_(b, c, limit, a, value=a, cardinality=limit])")
     [($in :a 0 5)
      ($in :b 0 5)
      ($in :c 0 5)
      ($in :limit 0 3)
      ($count 3 [:a :b :c] :limit)
      ($count :a [:b :c] :limit)]))

  (testing "among"
    (constraints-assert
     '("AMONG ([AMONG([a = {0..5},b = {0..5},c = {0..5}],{[2, 3]},3 = 3)])")
     [($in :a 0 5)
      ($in :b 0 5)
      ($in :c 0 5)
      ($in :limit 0 3)
      ($among 3 [:a :b :c] [2 3])]))

  (testing "at-least-n-values"
    (constraints-assert
     '("ATLEASTNVALUES ([PropAtLeastNValues(a, b, n-values)])")
     [($in :a 0 5)
      ($in :b 0 5)
      ($in :n-values 0 5)
      ($at-least-n-values [:a :b] :n-values)]
     ))

  (testing "at-most-n-values"
    (constraints-assert
     '("ATMOSTNVALUES ([PropAtMostNValues(a, b, n-values)])")
     [($in :a 0 5)
      ($in :b 0 5)
      ($in :n-values 0 5)
      ($at-most-n-values [:a :b] :n-values)]
     ))

  (testing "bin-packing"
    (constraints-assert
     '("BINPACKING ([PropItemToLoad(i1-bin, i2-bin, i3-bin, ..., bin-load-2), PropLoadToItem(bin-load-1, bin-load-2, i1-bin, ..., i3-bin), bin-load-1 = {0..2} + bin-load-2 = {0..5} = 6])")
     [($in :i1-bin 0 2)
      ($in :i2-bin 0 2)
      ($in :i3-bin 0 2)
      ($in :bin-load-1 0 2)
      ($in :bin-load-2 0 5)
      ($bin-packing
       {:i1-bin 1
        :i2-bin 2
        :i3-bin 3}
       [:bin-load-1 :bin-load-2]
       )]))

  (testing "bits-channeling"
    (constraints-assert
     '("BITSINTCHANNELING ([PropBitChanneling(int-var, b1, b2, ..., b4)])")
     [($in :int-var 0 16)
      ($bits-channeling [:b1 :b2 :b3 :b4] :int-var)]))
)

(deftest logic-compile-test
  (testing "and"
    (constraints-assert
     '("SUM ([b3 + b2 + b1 = IV_1 + 0])" "ARITHM ([IV_1 = 3])")
     [($bool :b1)
      ($bool :b2)
      ($bool :b3)
      ($and :b1 :b2 :b3)]
     "should handle list of booleans")

    (constraints-assert
     '("REIF_1 = [0,1]=>TRUE ([true]), !REIF_1 = [0,1]=>OPPOSITE ([PropOpposite(cste -- 1)])"
       "REIF_2 = [0,1]=>FALSE ([false]), !REIF_2 = [0,1]=>OPPOSITE ([PropOpposite(cste -- 0)])"
       "SUM ([PropXplusYeqZ(REIF_1, REIF_2, IV_3)])"
       "ARITHM ([IV_3 = 2])")
     [($and ($true) ($false))]
     )
    )

  (testing "or"
    (constraints-assert
     '("SUM ([b3 + b2 + b1 = IV_1 + 0])" "ARITHM ([IV_1 >= 1])")
     [($bool :b1)
      ($bool :b2)
      ($bool :b3)
      ($or :b1 :b2 :b3)]
     "should handle list of booleans")

    (constraints-assert
     '("REIF_1 = [0,1]=>TRUE ([true]), !REIF_1 = [0,1]=>OPPOSITE ([PropOpposite(cste -- 1)])"
       "REIF_2 = [0,1]=>FALSE ([false]), !REIF_2 = [0,1]=>OPPOSITE ([PropOpposite(cste -- 0)])"
       "SUM ([PropXplusYeqZ(REIF_1, REIF_2, IV_3)])"
       "ARITHM ([IV_3 >= 1])")
     [($or ($true) ($false))]
     )
    )

  (testing "when"
    (constraints-assert
     '("REIF_1 = [0,1]=>FALSE ([false]), !REIF_1 = [0,1]=>OPPOSITE ([PropOpposite(cste -- 0)])"
       "ARITHM ([REIF_1 = 0])")
     [($when ($false) ($false))])

    (constraints-assert
     '("ARITHM ([when = 0])")
     [($bool :when)
      ($when :when ($false))])

    (constraints-assert
     '("REIF_1 = [0,1]=>TRUE ([true]), !REIF_1 = [0,1]=>OPPOSITE ([PropOpposite(cste -- 1)])"
       "ARITHM ([prop(REIF_1.GEQ.when)])")
     [($bool :when)
      ($when :when ($true))])
    )

  (testing "if-then-else"
    (constraints-assert
     '("REIF_1 = [0,1]=>TRUE ([true]), !REIF_1 = [0,1]=>OPPOSITE ([PropOpposite(cste -- 1)])"
       "REIF_2 = [0,1]=>TRUE ([true]), !REIF_2 = [0,1]=>OPPOSITE ([PropOpposite(cste -- 1)])"
       "ARITHM ([prop(REIF_2.GEQ.REIF_1)])"
       "ARITHM ([not(REIF_1) = 0])")
     [($if ($true) ($true) ($false))])

    (constraints-assert
     '("REIF_1 = [0,1]=>TRUE ([true]), !REIF_1 = [0,1]=>OPPOSITE ([PropOpposite(cste -- 1)])"
       "ARITHM ([prop(REIF_1.GEQ.if)])"
       "ARITHM ([not(if) = 0])")
     [($bool :if)
      ($if :if ($true) ($false))])
    )

  #_(testing "cond"
    (constraints-assert
     '()
     [($cond
       ($false) ($true)
       ($false) ($false)
       ($true) ($true)
       ($false) ($true)
       :else ($true))])
    )

  (testing "not"
    (constraints-assert
     '("OPPOSITE ([PropOpposite(cste -- 0)])")
     [($not ($false))])

    (constraints-assert
     '("TRUE ([true])")
     [($not ($not ($true)))])

    (constraints-assert
     '("ARITHM ([prop(b.GEQ.a)])"
       "ARITHM ([prop(a.NEQ.b)])"
       "SUM ([a + b + c != 5])")
     [($in :a 0 5)
      ($in :b 0 5)
      ($in :c 0 1)
      ($not ($> :a :b))
      ($not ($= :a :b))
      ($not ($sum 5 := [:a :b :c]))]))

  (testing "true false"
    (constraints-assert
     '("TRUE ([true])")
     [($true)])

    (constraints-assert
     '("FALSE ([false])")
     [($false)]))
  )

(deftest reify-compile-test
  (constraints-assert
   '("SUM ([PropXplusYeqZ(b, c, IV_1)])"
     "a = [0,1]=>ARITHM ([IV_1 = 2]), !a = [0,1]=>ARITHM ([IV_1 =/= 2])")
   [($bool :b)
    ($bool :c)
    ($reify :a ($and :b :c))])

  (constraints-assert
   '("a = [0,1]=>ARITHM ([prop(c.EQ.b+2)]), !a = [0,1]=>ARITHM ([prop(c.NEQ.b+2)])"
     "ARITHM ([prop(b.EQ.b+2+-2)])")
   [($in :b 0 2)
    ($in :c 0 2)
    ($reify :a ($= :c ($+ :b 2)))])
  )
