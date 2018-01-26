(ns ^:model loco.model.basics
  (:use clojure.test
        loco.constraints)
  (:require [loco.model :as model]))

(deftest object-var-name-test
  (testing "var names that are not keywords should get converted into strings"
    (are [expected input] (= expected (model/compile input))

      [[:var "[:public]" :public [:const 1]]]
      [($in [:public] [1])]

      [[:var "[:public]" :public [:const 1]]
       [:var "[:public]+[:public]" :proto [:int 2 2]]
       [:constraint ['sum ["[:public]+[:public]" '= ["[:public]" "[:public]"]]]]
       [:constraint ['arithm ["[:public]" '= "[:public]+[:public]"]]]]
      [($in [:public] [1])
       ($= [:public] ($+ [:public] [:public]))]

      [[:var "[:p 0 1]" :public [:const 0]]
       [:var "[:p 0 1]*[:p 0 1]" :proto [:int 0 0]]
       [:constraint
        ['times ["[:p 0 1]*[:p 0 1]" '= "[:p 0 1]" '* "[:p 0 1]"]]]
       [:constraint ['arithm ["[:p 0 1]" '= "[:p 0 1]*[:p 0 1]"]]]]
      [($in [:p 0 1] [0])
       ($= [:p 0 1] ($* [:p 0 1] [:p 0 1]))])
    )
  )

(def fib [1 2 3 5 8 13])

(deftest basics-test
  (testing "with removed vars/name-maker"
    (are [expected input] (= expected (model/compile input))

      [[:var :my-tuples :hidden [:tuples true [[1 2] [2 3] [3 4]]]]]
      [($tuples :my-tuples [[1 2] [2 3] [3 4]])]

      [[:var :my-task :public [:task [1 2] [2 3] [3 4]]]]
      [($task :my-task [1 2] [2 3] [3 4])]

      [[:var :my-task :public [:task :start :duration :end]]]
      [($task :my-task :start :duration :end)]

      [[:var :a :public [:const 1]]]
      [($in :a [1])]

      [[:var :public :hidden [:const 1]]]
      [($in- :public [1])]

      [[:var :public :public [:const 1]]]
      [($in :public [1])]

      [[:var :public :public [:const 1]]]
      [($in :public [1 1 1])]

      [[:var :public :hidden [:const 1]]]
      [($in- :public [1 1 1])]

      [[:var :x :public [:int 1 5]]
       [:var :y :public [:int 1 5]]
       [:var :z :public [:int 1 5]]
       [:var :a :public [:int 1 5]]
       [:var :b :public [:int 1 5]]
       [:var :2 :hidden [:const 2]]
       [:constraint [:all-equal [:z :2 :a :b]]]
       [:constraint [:not-all-equal [:x :y :z]]]]
      [($in :x 1 5)
       ($in :y 1 5)
       ($in :z 1 5)
       ($in :a 1 5)
       ($in :b 1 5)
       ($= :z 2 :a :b)
       ($!= :x :y :z)]

      [[:var :7 :hidden [:const 7]]]
      [($const- :7 7)]

      [[:var :7 :public [:const 7]]]
      [($const :7 7)]

      [[:var :i :public [:int 0 5]]]
      [($in :i 0 5)]

      [[:var :i :public [:int fib]]]
      [($in :i fib)]

      [[:var :a :public [:int fib]]
       [:var :b :public [:int fib]]
       [:var :c :public [:int fib]]
       [:var :b+c :proto [:int 2 26]]
       [:constraint ['sum [:b+c '= [:b :c]]]]
       [:constraint ['arithm [:a '= :b+c]]]]
      [($in :a fib)
       ($in :b fib)
       ($in :c fib)
       ($= :a ($+ :b :c))]

      [[:var :a :public [:int fib]]
       [:var :b :public [:int fib]]
       [:var :c :public [:int [1 2 3 5 8]]]
       [:var :b+c :proto [:int 2 (+ 13 8)]]
       [:constraint ['sum [:b+c '= [:b :c]]]]
       [:constraint ['arithm [:a '= :b+c]]]]
      [($in :a fib)
       ($in :b fib)
       ($in :c (butlast fib))
       ($= :a ($+ :b :c))]

      [[:var :i :public [:int 0 5]]
       [:var :1 :hidden [:const 1]]
       [:constraint ['arithm [:1 '= :i]]]]
      [($in :i 0 5)
       ($= 1 :i)]

      [[:var :i :public [:int 0 5]]
       [:var :1 :hidden [:const 1]]
       [:var :-i :proto [:int -5 0]]
       [:constraint ['arithm [:1 '= :-i]]]]
      [($in :i 0 5)
       ($= 1 ($neg :i))]

      [[:var :i :public [:int 0 5]]
       [:var :-i :proto [:int -5 0]]
       [:var :1 :hidden [:const 1]]
       [:constraint ['arithm [:1 '= :-i]]]]
      [($in :i 0 5)
       ($neg :-i :i)
       ($= 1 :-i)]

      )))
