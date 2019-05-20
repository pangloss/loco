(ns loco.constraints.sum-test
  (:require
   [clojure.test :refer :all]
   [loco.constraints :refer :all]
   [loco.constraints.test-utils :refer :all]
   ))


      ;; [[:var :x :public [:int 0 5]]
      ;;  [:var :y :public [:int 0 5]]
      ;;  [:var :10 :hidden [:const 10]]
      ;;  [:var :5 :hidden [:const 5]]
      ;;  [:constraint ['sum [:10 '= [:x :y :5]]]]]
      ;; [($in :x 0 5)
      ;;  ($in :y 0 5)
      ;;  ($= 10 ($- :y :x 5))]

      ;;TODO: maybe it's better to make a boolean namespace instead of doing crazy overloading
      ;;not converted, not sure if it should be...
      ;; [[:var :x :public [:bool 0 1]]
      ;;  [:var :y :public [:bool 0 1]]
      ;;  [:var :z :public [:bool 0 1]]
      ;;  [:var :1 :hidden [:const 1]]
      ;;  [:constraint ['sum [:1 '= [:x :y :z]]]]]
      ;; [($bool :x)
      ;;  ($bool :y)
      ;;  ($bool :z)
      ;;  ($sum 1 := [:x :y :z])]

      ;;TODO: maybe it's better to put this into a set namespace, and not do crazy overloading like this. it would make for better documentation, at the least... maybe
      ;; [($in :sum 0 10)
      ;;  ($set :set [0 1 2] [0 1 2 3 4 5 6 7])
      ;;  ($sum :sum :set)]


(deftest sum-test

  (testing "$sum constructors"
    (is
     (loco?
      [($in :x 0 5)
       ($in :y 0 5)
       ($sum 10 := [10 10])
       ($sum 10 := [])
       ($sum 10 := [:x])
       ($sum 10 := [:x 4])
       ($sum 10 := [:x :y])
       ($sum 10 := [:x :y 5 4])
       ]
      {:model
       '[[:var :x :public [:int 0 5]]
         [:var :y :public [:int 0 5]]
         [:view "x+4" [offset :x [4]] [:int 0 5]]
         [arithm [10 = 20]]
         [arithm [10 = :x]]
         [arithm [10 = "x+4"]]
         [sum [10 = [:x :y]]]
         [sum [10 = [:x :y 9]]]]}
      ))

    (is
     (loco?
      [($in :x 0 5)
       ($in :y 0 5)
       ($sum 10 := [:x :y 5])
       ($sum 10 :> [:x :y 5])
       ($sum 10 :< [:x :y 5])
       ($sum 10 :>= [:x :y 5])
       ($sum 10 :<= [:x :y 5])
       ($sum 10 :!= [:x :y 5])]
      {:model
       '[[:var :x :public [:int 0 5]]
         [:var :y :public [:int 0 5]]
         [sum [10 = [:x :y 5]]]
         [sum [10 > [:x :y 5]]]
         [sum [10 < [:x :y 5]]]
         [sum [10 >= [:x :y 5]]]
         [sum [10 <= [:x :y 5]]]
         [sum [10 != [:x :y 5]]]]}
      )))

  (testing "booleans"
    ;;TODO: test with bool-ish constraints via reify
    (is
     (loco?
      [($bool :x)
       ($bool :y)
       ($bool :z)
       ($sum 1 := [:x :y :z])]
      {:model
       '[[:var :x :public [:bool 0 1]]
         [:var :y :public [:bool 0 1]]
         [:var :z :public [:bool 0 1]]
         [sum [1 = [:x :y :z]]]],
       :compiled
       [["x = [0,1]" "y = [0,1]" "z = [0,1]"]
        ["SUM ([z + y + x = 1])"]],
       :solutions
       #{{:x 1, :y 0, :z 0} {:x 0, :y 1, :z 0} {:x 0, :y 0, :z 1}}}
      ))

    )
  
  (testing "$sum set"
    (is
     (loco?
      [($in :sum 0 10)
       ($set :set [0 1 2] [0 1 2 3 4 5 6 7])
       ($sum :sum :set)]
      {:model
       '[[:var :sum :public [:int 0 10]]
         [:var :set :public [:set #{0 1 2} #{0 1 2 3 4 5 6 7}]]
         [sum [:sum = :set]]],
       :compiled
       [["sum = {0..10}"
         "set = [{0, 1, 2}, {0, 1, 2, 3, 4, 5, 6, 7}]"]
        ["SETSUM ([PropSumOfElements(set, sum)])"]],
       :solutions
       #{{:sum 9, :set #{0 1 6 2}} {:sum 7, :set #{0 1 4 2}}
         {:sum 8, :set #{0 1 2 5}} {:sum 10, :set #{0 7 1 2}}
         {:sum 10, :set #{0 1 4 3 2}} {:sum 6, :set #{0 1 3 2}}
         {:sum 3, :set #{0 1 2}}}}
      ))
    ))

