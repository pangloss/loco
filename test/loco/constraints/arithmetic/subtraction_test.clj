(ns loco.constraints.arithmetic.subtraction-test
  (:require
   [loco.constraints :refer :all]
   [clojure.test :refer :all]
   [loco.constraints.test-utils :refer :all]
   ))

(deftest subtration-test
  (is (loco?
       [($in :x 0 5)
        ($in :y -5 0)
        ($= :x ($- :y))]
       {:identity '[[:var :x :public [:int 0 5]]
                    [:var :y :public [:int -5 0]]
                    [arithm [:x = [minus :y []]]]],
        :model '[[:var :x :public [:int 0 5]]
                 [:var :y :public [:int -5 0]]
                 [:view "-y" [minus :y []] [:int 0 5]]
                 [arithm [:x = "-y"]]],
        :compiled [["x = {0..5}"
                    "y = {-5..0}"
                    "-(y = {-5..0}) = [0,5]"]
                   ["ARITHM ([prop(x.EQ.-(y))])"]],
        :solutions #{{:x 0, :y 0}
                     {:x 5, :y -5}
                     {:x 4, :y -4}
                     {:x 1, :y -1}
                     {:x 2, :y -2}
                     {:x 3, :y -3}}}))
  
  (is (loco?
       [($in :x 0 5)
        ($in :y 0 5)
        ($= :x ($- :y))]
       {:identity '[[:var :x :public [:int 0 5]]
                    [:var :y :public [:int 0 5]]
                    [arithm [:x = [minus :y []]]]]
        :model '[[:var :x :public [:int 0 5]]
                 [:var :y :public [:int 0 5]]
                 [:view "-y" [minus :y []] [:int -5 0]]
                 [arithm [:x = "-y"]]]
        :compiled [["x = {0..5}"
                    "y = {0..5}"
                    "-(y = {0..5}) = [-5,0]"]
                   ["ARITHM ([prop(x.EQ.-(y))])"]]
        :solutions #{{:x 0, :y -0}}}))

  (is
   (loco?
    [($in :g 0 5)
     ($in :h 0 5)
     ($in :j 1 2)
     ($= 0 ($- :g :h :j))]
    {:identity '[[:var :g :public [:int 0 5]]
                 [:var :h :public [:int 0 5]]
                 [:var :j :public [:int 1 2]]
                 [arithm [0 = [- [:g :h :j]]]]]
     :model '[[:var :g :public [:int 0 5]]
              [:var :h :public [:int 0 5]]
              [:var :j :public [:int 1 2]]
              [:var ":g-:h-:j" :proto [:int -7 4]]
              [:view "-h" [minus :h []] [:int -5 0]]
              [:view "-j" [minus :j []] [:int -2 -1]]
              [sum [":g-:h-:j" = [:g "-h" "-j"]]]
              [arithm [0 = ":g-:h-:j"]]]
     :compiled [["g = {0..5}"
                 "h = {0..5}"
                 "j = {1..2}"
                 ":g-:h-:j = {-7..4}"
                 "-(h = {0..5}) = [-5,0]"
                 "-(j = {1..2}) = [-2,-1]"]
                ["SUM ([-(j) + -(h) + g - :g-:h-:j = 0])"
                 "ARITHM ([:g-:h-:j = 0])"]]
     :solutions #{{:g 2, :h 0, :j 2}
                  {:g 3, :h 1, :j 2}
                  {:g 5, :h 3, :j 2}
                  {:g 4, :h 2, :j 2}
                  {:g 5, :h 4, :j 1}
                  {:g 4, :h 3, :j 1}
                  {:g 3, :h 2, :j 1}
                  {:g 1, :h 0, :j 1}
                  {:g 2, :h 1, :j 1}}}))

  (is
   (loco?
    [($in :x 0 5)
     ($in :y 0 5)
     ($in :z 1 2)
     ($= :x ($- :y ($- :x :z)))]
    {:identity '[[:var :x :public [:int 0 5]]
                 [:var :y :public [:int 0 5]]
                 [:var :z :public [:int 1 2]]
                 [arithm [:x = [- [:y [- [:x :z]]]]]]]
     :model '[[:var :x :public [:int 0 5]]
              [:var :y :public [:int 0 5]]
              [:var :z :public [:int 1 2]]
              [:var ":x-:z" :proto [:int -2 4]]
              [:view "-z" [minus :z []] [:int -2 -1]]
              [:var ":y-:x-:z" :proto [:int -4 7]]
              [:view "-:x-:z" [minus ":x-:z" []] [:int -4 2]]
              [sum [":x-:z" = [:x "-z"]]]
              [sum [":y-:x-:z" = [:y "-:x-:z"]]]
              [arithm [:x = ":y-:x-:z"]]]
     :compiled [["x = {0..5}"
                 "y = {0..5}"
                 "z = {1..2}"
                 ":x-:z = {-2..4}"
                 "-(z = {1..2}) = [-2,-1]"
                 ":y-:x-:z = {-4..7}"
                 "-(:x-:z = {-2..4}) = [-4,2]"]
                ["SUM ([PropXplusYeqZ(x, -(z), :x-:z)])"
                 "SUM ([PropXplusYeqZ(y, -(:x-:z), :y-:x-:z)])"
                 "ARITHM ([prop(x.EQ.:y-:x-:z)])"]]
     :solutions #{{:x 1, :y 0, :z 2}
                  {:x 2, :y 2, :z 2}
                  {:x 3, :y 4, :z 2}
                  {:x 1, :y 1, :z 1}
                  {:x 2, :y 3, :z 1}
                  {:x 3, :y 5, :z 1}}}
    ))

  (is
   (loco?
    [($in :a 0 5)
     ($in :b 0 5)
     ($= ($minus :a) ($- :b))]
    {:identity '[[:var :a :public [:int 0 5]]
                 [:var :b :public [:int 0 5]]
                 [arithm [[minus :a []] = [minus :b []]]]]
     :model '[[:var :a :public [:int 0 5]]
              [:var :b :public [:int 0 5]]
              [:view "-a" [minus :a []] [:int -5 0]]
              [:view "-b" [minus :b []] [:int -5 0]]
              [arithm ["-a" = "-b"]]]
     :compiled [["a = {0..5}"
                 "b = {0..5}"
                 "-(a = {0..5}) = [-5,0]"
                 "-(b = {0..5}) = [-5,0]"]
                ["ARITHM ([prop(-(a).EQ.-(b))])"]]
     :solutions #{{:a 5, :b 5}
                  {:a 4, :b 4}
                  {:a 0, :b 0}
                  {:a 3, :b 3}
                  {:a 2, :b 2}
                  {:a 1, :b 1}}}
    ))
  )
