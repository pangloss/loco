(ns loco.constraints.arithmetic.addition-test
  (:require
   [clojure.test :refer :all]
   [loco.constraints :refer :all]
   [loco.constraints.test-utils :refer :all]
   ))

(deftest addition-partial-test
  (is (loco?
       [($in :x 0 5)
        ($in :y -5 0)
        ($= :x ($+ :y))]
       {:identity '[[:var :x :public [:int 0 5]]
                    [:var :y :public [:int -5 0]]
                    [arithm [:x = [+ [:y]]]]],
        :model '[[:var :x :public [:int 0 5]]
                 [:var :y :public [:int -5 0]]
                 [:var ":y" :proto [:int -5 0]]
                 [sum [":y" = [:y]]]
                 [arithm [:x = ":y"]]],
        :compiled [["x = {0..5}" "y = {-5..0}" ":y = {-5..0}"]
                   ["ARITHM ([prop(y.EQ.:y)])" "ARITHM ([prop(x.EQ.:y)])"]],
        :solutions #{{:x 0, :y 0}}}))
  
  (is (loco?
       [($in :x 0 5)
        ($in :y 0 5)
        ($= :x ($+ :y))]
       {:identity '[[:var :x :public [:int 0 5]]
                    [:var :y :public [:int 0 5]]
                    [arithm [:x = [+ [:y]]]]],
        :model '[[:var :x :public [:int 0 5]]
                 [:var :y :public [:int 0 5]]
                 [:var ":y" :proto [:int 0 5]]
                 [sum [":y" = [:y]]]
                 [arithm [:x = ":y"]]],
        :compiled [["x = {0..5}" "y = {0..5}" ":y = {0..5}"]
                   ["ARITHM ([prop(y.EQ.:y)])" "ARITHM ([prop(x.EQ.:y)])"]],
        :solutions #{{:x 0, :y 0} {:x 1, :y 1} {:x 2, :y 2} {:x 4, :y 4}
                     {:x 3, :y 3} {:x 5, :y 5}}}))

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
      [:var ":g+:h+:j+0" :proto [:int 1 12]]
      [sum [":g+:h+:j+0" = [:g :h :j 0]]]
      [arithm [12 = ":g+:h+:j+0"]]],
     :compiled
     [["g = {0..5}"
       "h = {0..5}"
       "j = {1..2}"
       ":g+:h+:j+0 = {1..12}"]
      ["SUM ([j + h + g - :g+:h+:j+0 = 0])"
       "ARITHM ([:g+:h+:j+0 = 12])"]],
     :solutions #{{:g 5, :h 5, :j 2}}}
    ))

  (is
   (loco?
    [($in :x 0 5)
     ($in :y 0 5)
     ($in :z 1 2)
     ($= 10 ($+ :y ($+ :x :z)))]
    {:identity '[[:var :x :public [:int 0 5]]
                 [:var :y :public [:int 0 5]]
                 [:var :z :public [:int 1 2]]
                 [arithm [10 = [+ [:y [+ [:x :z]]]]]]],
     :model '[[:var :x :public [:int 0 5]]
              [:var :y :public [:int 0 5]]
              [:var :z :public [:int 1 2]]
              [:var ":x+:z" :proto [:int 1 7]]
              [:var ":y+:x+:z" :proto [:int 1 12]]
              [sum [":x+:z" = [:x :z]]]
              [sum [":y+:x+:z" = [:y ":x+:z"]]]
              [arithm [10 = ":y+:x+:z"]]],
     :compiled [["x = {0..5}"
                 "y = {0..5}"
                 "z = {1..2}"
                 ":x+:z = {1..7}"
                 ":y+:x+:z = {1..12}"]
                ["SUM ([PropXplusYeqZ(x, z, :x+:z)])"
                 "SUM ([PropXplusYeqZ(y, :x+:z, :y+:x+:z)])"
                 "ARITHM ([:y+:x+:z = 10])"]],
     :solutions
     #{{:x 3, :y 5, :z 2} {:x 4, :y 4, :z 2} {:x 4, :y 5, :z 1}
       {:x 5, :y 3, :z 2} {:x 5, :y 4, :z 1}}}
    ))

  (is
   (loco?
    [($in :a 0 5)
     ($in :b 0 5)
     ($= ($minus :a) ($+ :b))]
    {:identity '[[:var :a :public [:int 0 5]]
                 [:var :b :public [:int 0 5]]
                 [arithm [[minus :a []] = [+ [:b]]]]],
     :model '[[:var :a :public [:int 0 5]]
              [:var :b :public [:int 0 5]]
              [:view "-a" [minus :a []] [:int -5 0]]
              [:var ":b" :proto [:int 0 5]]
              [sum [":b" = [:b]]]
              [arithm ["-a" = ":b"]]],
     :compiled [["a = {0..5}"
                 "b = {0..5}"
                 "-(a = {0..5}) = [-5,0]"
                 ":b = {0..5}"]
                ["ARITHM ([prop(b.EQ.:b)])"
                 "ARITHM ([prop(-(a).EQ.:b)])"]],
     :solutions #{{:a 0, :b 0}}}
    ))

 )
