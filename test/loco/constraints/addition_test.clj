(ns loco.constraints.addition-test
  (:require
   [clojure.test :refer :all]))

(deftest addition-test
  (testing "$subtration"
    (are [expected input] (= expected input)
      '[[:var :x :public [:int 0 5]]
        [:var :y :public [:int 0 5]]
        [arithm [:x = [minus :y []]]]]
      [($in :x 0 5)
       ($in :y 0 5)
       ($= :x ($+ :y))]
      ))

  (testing "model/compile"
    (are [expected input] (= expected (model/compile input))
      '[[:var :x :public [:int 0 5]]
        [:var :y :public [:int 0 5]]
        [:view "-y" [minus :y []] [:int -5 0]]
        [arithm [:x = "-y"]]]
      [($in :x 0 5)
       ($in :y 0 5)
       ($= :x ($+ :y))]

      '[[:var :x :public [:int 0 5]]
        [:var :y :public [:int 0 5]]
        [:var :z :public [:int 1 2]]
        [:var ":x+:z" :proto [:int 1 7]]
        [:var ":y-:x+:z" :proto [:int -7 4]]
        [:view "-:x+:z" [minus ":x+:z" []] [:int -7 -1]]
        [sum [":x+:z" = [:x :z]]]
        [sum [":y-:x+:z" = [:y "-:x+:z"]]]
        [arithm [:x = ":y-:x+:z"]]]
      [($in :x 0 5)
       ($in :y 0 5)
       ($in :z 1 2)
       ($= :x ($+ :y ($+ :x :z)))]
      )
    )

  (testing "compiler/compile"
    (choco-vars-string-assert
     '("x = {0..5}" "y = {0..5}" "-(y = {0..5}) = [-5,0]")
      [($in :x 0 5)
       ($in :y 0 5)
       ($= :x ($- :y))]
      ))

  (testing "solver/solutions"
    (are [expected input] (= expected (solver/solutions input))
      '({:x 5, :y 5}
        {:x 4, :y 4}
        {:x 0, :y 0}
        {:x 3, :y 3}
        {:x 2, :y 2}
        {:x 1, :y 1})
      [($in :x 0 5)
       ($in :y 0 5)
       ($= ($minus :x) ($- :y))]

      '({:x 0, :y 1, :z 1}
        {:x 2, :y 5, :z 1}
        {:x 1, :y 3, :z 1}
        {:x 0, :y 2, :z 2}
        {:x 1, :y 4, :z 2})
      [($in :x 0 5)
       ($in :y 0 5)
       ($in :z 1 2)
       ($= :x ($- :y ($+ :x :z)))]
      ))

  )
