(ns loco.constraints.partials-test
  (:require [loco.model :as model]
            [loco.compiler :as compiler]
            [loco.solver :as solver])
  (:use
   loco.constraints
   clojure.test))

(deftest ^:model arith-inside-view-test
  (are [expected input] (= expected (->> input model/compile))
    '[[:var :x :public [:int -5 5]]
      [:var :y :public [:int 0 2]]
      [:var "x*y" :proto [:int -25 10]]
      [:var "-x*y" :proto [:int -10 25]]
      [times ["x*y" '= :x '* :y]]
      [arithm [1 = [minus-view "x*y" []]]]]
    [($in :x -5 5)
     ($in :y 0 2)
     ($= 1 ($neg ($* :x :y)))]
    ))

(deftest ^:model partials-test
  (are [expected input] (= expected (->> input model/compile))
    '[[:var :x :public [:int 0 5]]
      [:var :y :public [:int 0 5]]
      [:var "y-x" :proto [:int -5 5]]
      [:view "-x" [minus-view :x []] [:int -5 0]]
      [:var "x-y-x" :proto [:int -5 10]]
      [:view "-y-x" [minus-view "y-x" []] [:int -5 5]]
      [sum ["y-x" = [:y "-x"]]]
      [sum ["x-y-x" = [:x "-y-x"]]]
      [arithm [5 = "x-y-x"]]]
    [($in :x 0 5)
     ($in :y 0 5)
     ($= 5 ($- :x ($- :y :x)))]

    '[[:var :x :public [:int 0 5]]
      [:var :y :public [:int 0 5]]
      [:var "y-x-7" :proto [:int -12 -2]]
      [:view "-x" [minus-view :x []] [:int -5 0]]
      [:var "x-3-y-x-7" :proto [:int -1 14]]
      [:view "-y-x-7" [minus-view "y-x-7" []] [:int 2 12]]
      [sum ["y-x-7" = [:y "-x" -7]]]
      [sum ["x-3-y-x-7" = [:x "-y-x-7" -3]]]
      [arithm [5 = "x-3-y-x-7"]]]
    [($in :x 0 5)
     ($in :y 0 5)
     ($= 5 ($- :x 3 ($- :y :x 7)))]

    [[:var :x :public [:int 0 5]]
     [:var :y :public [:int 0 5]]
     [:var "x+y+5" :proto [:int 5 15]]
     ['sum ["x+y+5" '= [:x :y 5]]]
     ['arithm [10 '= "x+y+5"]]]
    [($in :x 0 5)
     ($in :y 0 5)
     ($= 10 ($+ :x :y 5))]

    [[:var :x :public [:int 0 5]]
     [:var :y :public [:int 0 5]]
     [:var "x*y" :proto [:int 0 25]]
     ['times ["x*y" '= :x '* :y]]
     ['arithm [0 '= "x*y"]]]
    [($in :x 0 5)
     ($in :y 0 5)
     ($= 0 ($* :x :y))]

    [[:var :x :public [:int -5 5]]
     [:var :y :public [:int 0 5]]
     [:var "x*y" :proto [:int -25 25]]
     ['times ["x*y" '= :x '* :y]]
     ['arithm [0 '= "x*y"]]]
    [($in :x -5 5)
     ($in :y 0 5)
     ($= 0 ($* :x :y))]

    [[:var :x :public [:int 5 5]]
     [:var :y :public [:int 0 2]]
     [:var "x/y" :proto [:int 2 5]]
     ['div ["x/y" '= :x '/ :y]]
     ['arithm [0 '= "x/y"]]]
    [($in :x 5 5)
     ($in :y 0 2)
     ($= 0 ($div :x :y))]

    [[:var :x :public [:int 5 5]]
     [:var :y :public [:int -2 0]]
     [:var "x/y" :proto [:int -5 -2]]
     ['div ["x/y" '= :x '/ :y]]
     ['arithm [0 '= "x/y"]]]
    [($in :x  5 5)
     ($in :y -2 0)
     ($= 0 ($div :x :y))]

    [[:var :x :public [:int -5 5]]
     [:var :y :public [:int 2 2]]
     [:var "x/y" :proto [:int -2 2]]
     ['div ["x/y" '= :x '/ :y]]
     ['arithm [0 '= "x/y"]]]
    [($in :x -5 5)
     ($in :y 2 2)
     ($= 0 ($div :x :y))]

    [[:var :_a :hidden [:int 1 1]]
     [:var :x :public [:int 0 5]]
     [:var :y :public [:int 0 5]]
     [:var "x-y" :proto [:int -5 5]]
     [:view "-y" ['minus-view :y []] [:int -5 0]]
     [:var "y-x" :proto [:int -5 5]]
     [:view "-x" ['minus-view :x []] [:int -5 0]]
     ['sum ["x-y" '= [:x "-y"]]]
     ['sum ["y-x" '= [:y "-x"]]]
     ['= [5 "x-y" "y-x"]]]
    [($in :_a 1)
     ($in :x 0 5)
     ($in :y 0 5)
     ($= 5 ($- :x :y) ($- :y :x))]

    [[:var :x :public [:int 0 5]]
     [:view "x-5" ['offset-view :x [-5]] [:int -5 0]]
     ['arithm ["x-5" '= 0]]]
    [($in :x 0 5)
     ($= ($- :x 5) 0)]

    [[:var :x :public [:bool 0 1]]
     [:var :y :public [:bool 0 1]]
     [:var "x+y" :proto [:int 0 2]]
     ['sum ["x+y" '= [:x :y]]]
     ['arithm [2 '= "x+y"]]]
    [($bool :x)
     ($bool :y)
     ($= 2 ($+ :x :y))]

    [[:var :a :public [:int 0 362880]]
     ['arithm [:a '= 362880]]]
    [($in :a 0 (* 1 2 3 4 5 6 7 8 9))
     ($= :a ($* 1 2 3 4 5 6 7 8 9))]

    [[:var :x :public [:int 0 100]]
     [:var :y :hidden [:const 10]]
     [:var :z :public [:int 0 5]]
     [:var "x%y" :proto [:int 0 10]]
     ['mod ["x%y" '= :x '% :y]]
     ['arithm [:z '= "x%y"]]]
    [($in :x 0 100)
     ($const- :y 10)
     ($in :z 0 5)
     ($= :z ($% :x :y))]))
