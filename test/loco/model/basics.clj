(ns ^:model loco.model.basics
  (:use clojure.test
        loco.model.test
        loco.constraints))

(deftest object-var-name-test
  (testing "var names that are not keywords should get converted into strings"
    (compiled-assert
     [[:var "[:public]" :public [:int 1 1]]]
     [($in [:public] [1])])

    (compiled-assert
     [[:var "[:public]" :public [:int 1 1]]
      [:var "[:public]+[:public]" :proto [:int 2 2]]
      [:constraint [:sum ["[:public]+[:public]" := ["[:public]" "[:public]"]]]]
      [:constraint [:arithm ["[:public]" := "[:public]+[:public]"]]]]
     [($in [:public] [1])
      ($= [:public] ($+ [:public] [:public]))])

    (compiled-assert
     [[:var "[:p 0 1]" :public [:int 0 0]]
      [:var "[:p 0 1]*[:p 0 1]" :proto [:int 0 0]]
      [:constraint
       [:times ["[:p 0 1]*[:p 0 1]" := "[:p 0 1]" :* "[:p 0 1]"]]]
      [:constraint [:arithm ["[:p 0 1]" := "[:p 0 1]*[:p 0 1]"]]]]
     [($in [:p 0 1] [0])
      ($= [:p 0 1] ($* [:p 0 1] [:p 0 1]))])
    )
  )


(deftest basics-test
  (compiled-assert
   [[:var :a :public [:int 1 1]]]
   [($in :a [1])])

  (compiled-assert
   [[:var :public :hidden [:int 1 1]]]
   [($in- :public [1])])

  (compiled-assert
   [[:var :public :public [:int 1 1]]]
   [($in :public [1])])

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

  (def fib [1 2 3 5 8 13])

  (compiled-assert
   [[:var :i :public [:int fib]]]
   [($in :i fib)])

  (compiled-assert
   [[:var :a :public [:int fib]]
    [:var :b :public [:int fib]]
    [:var :c :public [:int fib]]
    [:var :b+c :proto [:int 2 26]]
    [:constraint [:sum [:b+c := [:b :c]]]]
    [:constraint [:arithm [:a := :b+c]]]]

   [($in :a fib)
    ($in :b fib)
    ($in :c fib)
    ($= :a ($+ :b :c))])

  (compiled-assert
   [[:var :a :public [:int [1 2 3 5 8 13]]]
    [:var :b :public [:int [1 2 3 5 8 13]]]
    [:var :c :public [:int [1 2 3 5 8]]]
    [:var :b+c :proto [:int 2 (+ 13 8)]]
    [:constraint [:sum [:b+c := [:b :c]]]]
    [:constraint [:arithm [:a := :b+c]]]]

   [($in :a fib)
    ($in :b fib)
    ($in :c (butlast fib))
    ($= :a ($+ :b :c))])

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
