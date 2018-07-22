(ns loco.constraints.arithmetic.subtraction-test
  (:require
   [loco.constraints :refer :all]
   [loco.model :as model]
   [loco.compiler :as compiler]
   [loco.solver :as solver]
   [clojure.test :refer :all]
   [loco.constraints.test-utils :refer :all]))

(defn multi-test [input]
  [input
   (model/compile input)
   (compiled-constraints-strings input)
   (compiled-vars-strings input)
   (solver/solutions input)])

(deftest subtraction-test
  (testing "$subtration"
    (are [expected-identity
          expected-model
          expected-compiled-constraints
          expected-compiled-vars
          expected-solutions
          input] (= [expected-identity
                     expected-model
                     expected-compiled-constraints
                     expected-compiled-vars
                     expected-solutions]
                    (multi-test input))
      '[[:var :x :public [:int 0 5]]
        [:var :y :public [:int 0 5]]
        [arithm [:x = [minus :y []]]]]
      '[[:var :x :public [:int 0 5]]
        [:var :y :public [:int 0 5]]
        [:view "-y" [minus :y []] [:int -5 0]]
        [arithm [:x = "-y"]]]
      '("ARITHM ([prop(x.EQ.-(y))])")
      '("x = {0..5}" "y = {0..5}" "-(y = {0..5}) = [-5,0]")
      '({:x 0, :y -0})
      [($in :x 0 5)
       ($in :y 0 5)
       ($= :x ($- :y))]

      '[[:var :a :public [:int 0 5]]
        [:var :b :public [:int 0 5]]
        [arithm [[minus :a []] = [minus :b []]]]]
      '[[:var :a :public [:int 0 5]]
        [:var :b :public [:int 0 5]]
        [:view "-a" [minus :a []] [:int -5 0]]
        [:view "-b" [minus :b []] [:int -5 0]]
        [arithm ["-a" = "-b"]]]
      '("ARITHM ([prop(-(a).EQ.-(b))])")
      '("a = {0..5}"
        "b = {0..5}"
        "-(a = {0..5}) = [-5,0]"
        "-(b = {0..5}) = [-5,0]")
      '({:a 5, :b 5}
        {:a 4, :b 4}
        {:a 0, :b 0}
        {:a 3, :b 3}
        {:a 2, :b 2}
        {:a 1, :b 1})
      [($in :a 0 5)
       ($in :b 0 5)
       ($= ($minus :a) ($- :b))]

      '[[:var :x :public [:int 0 5]]
        [:var :y :public [:int 0 5]]
        [:var :z :public [:int 1 2]]
        [arithm [:x = [- [:y [- [:x :z]]]]]]]
      '[[:var :x :public [:int 0 5]]
        [:var :y :public [:int 0 5]]
        [:var :z :public [:int 1 2]]
        [:var ":x-:z" :proto [:int -2 4]]
        [:view "-z" [minus :z []] [:int -2 -1]]
        [:var ":y-:x-:z" :proto [:int -4 7]]
        [:view "-:x-:z" [minus ":x-:z" []] [:int -4 2]]
        [sum [":x-:z" = [:x "-z"]]]
        [sum [":y-:x-:z" = [:y "-:x-:z"]]]
        [arithm [:x = ":y-:x-:z"]]]
      '("SUM ([PropXplusYeqZ(x, -(z), :x-:z)])"
        "SUM ([PropXplusYeqZ(y, -(:x-:z), :y-:x-:z)])"
        "ARITHM ([prop(x.EQ.:y-:x-:z)])")
      '("x = {0..5}"
        "y = {0..5}"
        "z = {1..2}"
        ":x-:z = {-2..4}"
        "-(z = {1..2}) = [-2,-1]"
        ":y-:x-:z = {-4..7}"
        "-(:x-:z = {-2..4}) = [-4,2]")
      '({:x 1, :y 0, :z 2}
        {:x 2, :y 2, :z 2}
        {:x 3, :y 4, :z 2}
        {:x 1, :y 1, :z 1}
        {:x 2, :y 3, :z 1}
        {:x 3, :y 5, :z 1})
      [($in :x 0 5)
       ($in :y 0 5)
       ($in :z 1 2)
       ($= :x ($- :y ($- :x :z)))]

            '[[:var :x :public [:int 0 5]]
        [:var :y :public [:int 0 5]]
        [:var :z :public [:int 1 2]]
        [arithm [:x = [- [:y [- [:x :z]]]]]]]
      '[[:var :x :public [:int 0 5]]
        [:var :y :public [:int 0 5]]
        [:var :z :public [:int 1 2]]
        [:var ":x-:z" :proto [:int -2 4]]
        [:view "-z" [minus :z []] [:int -2 -1]]
        [:var ":y-:x-:z" :proto [:int -4 7]]
        [:view "-:x-:z" [minus ":x-:z" []] [:int -4 2]]
        [sum [":x-:z" = [:x "-z"]]]
        [sum [":y-:x-:z" = [:y "-:x-:z"]]]
        [arithm [:x = ":y-:x-:z"]]]
      '("SUM ([PropXplusYeqZ(x, -(z), :x-:z)])"
        "SUM ([PropXplusYeqZ(y, -(:x-:z), :y-:x-:z)])"
        "ARITHM ([prop(x.EQ.:y-:x-:z)])")
      '("x = {0..5}"
        "y = {0..5}"
        "z = {1..2}"
        ":x-:z = {-2..4}"
        "-(z = {1..2}) = [-2,-1]"
        ":y-:x-:z = {-4..7}"
        "-(:x-:z = {-2..4}) = [-4,2]")
      '({:x 1, :y 0, :z 2}
        {:x 2, :y 2, :z 2}
        {:x 3, :y 4, :z 2}
        {:x 1, :y 1, :z 1}
        {:x 2, :y 3, :z 1}
        {:x 3, :y 5, :z 1})
      [($in :g 0 5)
       ($in :h 0 5)
       ($in :j 1 2)
       ($= 0 ($- :g :h :j))]
      ))

  )


(->> [($in :g 0 5)
      ($in :h 0 5)
      ($in :j 1 2)
      ($= 0 ($- :g :h :j))]
     model/compile
     ;;compiler/compile
     )
