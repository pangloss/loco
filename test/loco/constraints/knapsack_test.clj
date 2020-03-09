(ns loco.constraints.knapsack-test
  (:require
   [loco.model :as model]
   [loco.compiler :as compiler]
   [loco.solver :as solver]
   [loco.constraints.test-utils :refer :all])
  (:use
   loco.constraints
   clojure.test))

(deftest knapsack-test
  (testing "simple knapsack model"
    (test-loco
     [($in :x 1 2)
      ($in :y 1 2)
      ($in :z 1 2)
      ($in :W 0 200)
      ($in :V 0 200)
      ($knapsack [3 1 2]                ; weights
                 [5 6 7]                ; values
                 [:x :y :z]             ; occurrences
                 :W                     ; total weight
                 :V)]
     {
      :identity '[[:var :x :public [:int 1 2]]
                  [:var :y :public [:int 1 2]]
                  [:var :z :public [:int 1 2]]
                  [:var :W :public [:int 0 200]]
                  [:var :V :public [:int 0 200]]
                  [knapsack
                   [[weight [3 1 2]]
                    [energy [5 6 7]]
                    [occurrences [:x :y :z]]
                    [weight-sum :W]
                    [energy-sum :V]]]],
      :model '[[:var :x :public [:int 1 2]]
               [:var :y :public [:int 1 2]]
               [:var :z :public [:int 1 2]]
               [:var :W :public [:int 0 200]]
               [:var :V :public [:int 0 200]]
               [knapsack
                [[weight [3 1 2]]
                 [energy [5 6 7]]
                 [occurrences [:x :y :z]]
                 [weight-sum :W]
                 [energy-sum :V]]]],
      :compiled
      [["x = {1..2}"
        "y = {1..2}"
        "z = {1..2}"
        "W = {0..200}"
        "V = {0..200}"]
       ["KNAPSACK ([CSPLarge({x = {1..2}, , y = {1..2}, , z = {1..2}, , W = {0..200}, }), CSPLarge({x = {1..2}, , y = {1..2}, , z = {1..2}, , V = {0..200}, }), PropKnapsack(x, y, z, ..., V)])"]]
      :solutions #{{:x 1, :y 2, :z 1, :W 7, :V 24} {:x 2, :y 2, :z 2, :W 12, :V 36}
                   {:x 2, :y 1, :z 2, :W 11, :V 30} {:x 1, :y 1, :z 1, :W 6, :V 18}
                   {:x 1, :y 1, :z 2, :W 8, :V 25} {:x 1, :y 2, :z 2, :W 9, :V 31}
                   {:x 2, :y 2, :z 1, :W 10, :V 29} {:x 2, :y 1, :z 1, :W 9, :V 23}}
      }
     ))
  )
