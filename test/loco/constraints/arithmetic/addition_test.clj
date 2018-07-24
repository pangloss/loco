(ns loco.constraints.arithmetic.addition-test
  (:require
   [clojure.test :refer :all]
   [loco.constraints :refer :all]
   [loco.constraints.test-utils :refer :all]
   ))

(deftest addition-partial-test
  (testing "partial vars name generation"
    (is
     (loco?
      [($in :g 0 5)
       ($in :h 0 5)
       ($in :j 1 2)
       ($= 12 ($+ :g :h :j 0))]
      {:model '[[:var :g :public [:int 0 5]]
                [:var :h :public [:int 0 5]]
                [:var :j :public [:int 1 2]]
                [:var "g+h+j+0" :proto [:int 1 12]]
                [sum ["g+h+j+0" = [:g :h :j 0]]]
                [arithm [12 = "g+h+j+0"]]]}
      ))
    )

  (testing "should compress and use offset for raw numbers"
    (is (loco?
         [($in :x 0 5)
          ($= :x ($+ 1 1))]
         {:model '[[:var :x :public [:int 0 5]]
                   [arithm [:x = 2]]],
          :solutions #{{:x 2}}}))

    (is (loco?
         [($in :x 0 5)
          ($in :y -5 0)
          ($= :x ($+ :y 1))]
         {:model
          '[[:var :x :public [:int 0 5]]
            [:var :y :public [:int -5 0]]
            [:view "y+1" [offset :y [1]] [:int -5 0]]
            [arithm [:x = "y+1"]]],
          :solutions #{{:x 1, :y 0} {:x 0, :y -1}}}))

    (is (loco?
         [($in :x 0 5)
          ($in :y -5 0)
          ($= :x ($+ :y 1 2 3))]
         {:model
          '[[:var :x :public [:int 0 5]]
            [:var :y :public [:int -5 0]]
            [:view "y+6" [offset :y [6]] [:int -5 0]]
            [arithm [:x = "y+6"]]],
          :solutions
          #{{:x 1, :y -5} {:x 5, :y -1} {:x 3, :y -3} {:x 2, :y -4}
            {:x 4, :y -2}}}))

    )

  (testing "addition should handle 0 arity calls gracefully"
    (is (loco?
         [($in :x 0 1)
          ($in :y -1 0)
          ($= :x ($+ ))]
         {:model
          '[[:var :x :public [:int 0 1]]
            [:var :y :public [:int -1 0]]]
          :solutions
          #{{:x 0, :y 0} {:x 1, :y 0} {:x 0, :y -1} {:x 1, :y -1}}
          }
         )))

  (is (loco?
       [($in :x 0 5)
        ($in :y -5 0)
        ($= :x ($+ :y))]
       {:model
        '[[:var :x :public [:int 0 5]]
          [:var :y :public [:int -5 0]]
          [arithm [:x = :y]]],
        :solutions #{{:x 0, :y 0}}}))

  (is (loco?
       [($in :x 0 5)
        ($in :y -5 0)
        ($= :x ($+ ($+ :y :y)))]
       {:model
        '[[:var :x :public [:int 0 5]]
          [:var :y :public [:int -5 0]]
          [:var "y+y" :proto [:int -10 0]]
          [sum ["y+y" = [:y :y 0]]]
          [arithm [:x = "y+y"]]]}
       ))

  (is (loco?
       [($in :x 0 5)
        ($in :y -5 0)
        ($= :x ($+ ($+ :y :y 0)))]
       {:model
        '[[:var :x :public [:int 0 5]]
          [:var :y :public [:int -5 0]]
          [:var "y+y+0" :proto [:int -10 0]]
          [sum ["y+y+0" = [:y :y 0]]]
          [arithm [:x = "y+y+0"]]]}
       ))

  (testing "addition should handle 1 arity calls gracefully"
    (is (loco?
         [($in :x 0 5)
          ($in :y -5 0)
          ($= :x ($+ :y))]
         {:model
          '[[:var :x :public [:int 0 5]]
            [:var :y :public [:int -5 0]]
            [arithm [:x = :y]]],
          :solutions #{{:x 0, :y 0}}})))

  (is
   (loco?
    [($in :g 0 5)
     ($in :h 0 5)
     ($in :j 1 2)
     ($= 12 ($+ :g :h :j 0))]
    {:identity
     '[[:var :g :public [:int 0 5]]
       [:var :h :public [:int 0 5]]
       [:var :j :public [:int 1 2]]
       [arithm [12 = [+ [:g :h :j 0]]]]],
     :model
     '[[:var :g :public [:int 0 5]]
       [:var :h :public [:int 0 5]]
       [:var :j :public [:int 1 2]]
       [:var "g+h+j+0" :proto [:int 1 12]]
       [sum ["g+h+j+0" = [:g :h :j 0]]]
       [arithm [12 = "g+h+j+0"]]],
     :compiled
     [["g = {0..5}"
       "h = {0..5}"
       "j = {1..2}"
       "g+h+j+0 = {1..12}"]
      ["SUM ([j + h + g - g+h+j+0 = 0])"
       "ARITHM ([g+h+j+0 = 12])"]],
     :solutions #{{:g 5, :h 5, :j 2}}}
    ))

  (is
   (loco?
    [($in :x 0 5)
     ($in :y 0 5)
     ($in :z 1 2)
     ($= 10 ($+ :y ($+ :x :z)))]
    {:identity
     '[[:var :x :public [:int 0 5]]
       [:var :y :public [:int 0 5]]
       [:var :z :public [:int 1 2]]
       [arithm [10 = [+ [:y [+ [:x :z]]]]]]],
     :model
     '[[:var :x :public [:int 0 5]]
       [:var :y :public [:int 0 5]]
       [:var :z :public [:int 1 2]]
       [:var "x+z" :proto [:int 1 7]]
       [:var "y+x+z" :proto [:int 1 12]]
       [sum ["x+z" = [:x :z 0]]]
       [sum ["y+x+z" = [:y "x+z" 0]]]
       [arithm [10 = "y+x+z"]]],
     :compiled
     [["x = {0..5}"
       "y = {0..5}"
       "z = {1..2}"
       "x+z = {1..7}"
       "y+x+z = {1..12}"]
      ["SUM ([PropXplusYeqZ(x, z, x+z)])"
       "SUM ([PropXplusYeqZ(y, x+z, y+x+z)])"
       "ARITHM ([y+x+z = 10])"]],
     :solutions
     #{{:x 3, :y 5, :z 2} {:x 4, :y 4, :z 2} {:x 4, :y 5, :z 1}
       {:x 5, :y 3, :z 2} {:x 5, :y 4, :z 1}}}
    ))

  )
