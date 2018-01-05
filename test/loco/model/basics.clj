(ns ^:model loco.model.basics
  (:use clojure.test
        loco.model.test
        loco.constraints))

(deftest basics-test
  (compiled-assert
   [[:var :x :public [:int 1 5]]
    [:var :y :public [:int 1 5]]
    [:var :z :public [:int 1 5]]
    [:var :a :public [:int 1 5]]
    [:var :b :public [:int 1 5]]
    [:var :2 :hidden [:const 2]]
    [:constraint [:all-equal [:z :2 :a :b]]]
    [:constraint [:arithm [:z := 2]]]
    [:constraint [:arithm [:x :< :y]]]
    [:constraint [:arithm [:y :<= :z]]]
    [:constraint [:arithm [:y :> :x]]]
    [:constraint [:arithm [:z :>= :y]]]
    [:constraint [:arithm [:x :!= :y]]]
    [:constraint [:not-all-equal [:x :y :z]]]]

   [($in :x 1 5)
    ($in :y 1 5)
    ($in :z 1 5)
    ($in :a 1 5)
    ($in :b 1 5)
    ($= :z 2 :a :b)
    ($= :z 2)
    ($< :x :y)
    ($<= :y :z)
    ($> :y :x)
    ($>= :z :y)
    ($!= :x :y)
    ($!= :x :y :z)])

  (compiled-assert
   [[:var :7 :hidden [:const 7]]]

   [($const :7 7)])

  (compiled-assert
   [[:var :i :public [:int 0 5]]]

   [($in :i 0 5)])

  (compiled-assert
   [[:var :i :public [:int 0 5]]
    [:var :1 :hidden [:const 1]]
    [:constraint [:arithm [:1 := :i]]]]
   [($in :i 0 5)
    ($= 1 :i)])

  (compiled-assert
   [[:var :i :public [:int 0 5]]
    [:var :-i :proto [:int -5 0]]
    [:var :1 :hidden [:const 1]]
    [:constraint [:arithm [:1 := :-i]]]]

   [($in :i 0 5)
    ($neg :-i :i)
    ($= 1 :-i)])

  (compiled-assert
   [[:var :i :public [:int 0 5]]
    [:var :1 :hidden [:const 1]]
    [:var :-i :proto [:int -5 0]]
    [:constraint [:arithm [:1 := :-i]]]]

   [($in :i 0 5)
    ($= 1 ($neg :i))])

  (compiled-assert
   [[:var :i :public [:int 0 5]]
    [:var :-i :proto [:int -5 0]]
    [:var :1 :hidden [:const 1]]
    [:constraint [:arithm [:1 := :-i]]]]

   [($in :i 0 5)
    ($neg :-i :i)
    ($= 1 :-i)])

  )
