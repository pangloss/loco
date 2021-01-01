(ns loco.constraints.times-test
  (:require
   [clojure.test :refer :all]
   [loco.constraints :refer :all]
   [loco.constraints.test-utils :refer :all]))
   

(deftest times-test
  (testing "should use $scale view with numbers"
    (test-loco
     [($in :x 0 5)
      ($in :z 0 5)
      ($times :z :x 5)]
     {:model
      '[[:var :x :public [:int 0 5]]
        [:var :z :public [:int 0 5]]
        [:view "x*5" [scale :x [5]] [:int 0 25]]
        [arithm [:z = "x*5"]]]
      :compiled
      [["x = {0..5}"
        "z = {0..5}"
        "(x = {0..5} * 5) = [0,25]"]
       ["ARITHM ([prop(z.EQ.(x*5))])"]]
      :solutions
      #{{:x 0, :z 0} {:x 1, :z 5}}})))

(deftest times-test2
  (test-loco
   [($in :x 0 5)
    ($in :y 0 5)
    ($in :z 0 5)
    ($times :z :x :y)
    ($times :z = :x * :y)]
   {:model
    '[[:var :x :public [:int 0 5]]
      [:var :y :public [:int 0 5]]
      [:var :z :public [:int 0 5]]
      [times [:z = :x * :y]]],
    :compiled
    [["x = {0..5}"
      "y = {0..5}"
      "z = {0..5}"]
     ["TABLE ([CSPLarge({x = {0..5}, , y = {0..5}, , z = {0..5}, })])"]],
    :solutions
    #{{:x 1, :y 4, :z 4} {:x 0, :y 5, :z 0} {:x 0, :y 4, :z 0}
      {:x 1, :y 1, :z 1} {:x 3, :y 0, :z 0} {:x 1, :y 2, :z 2}
      {:x 2, :y 1, :z 2} {:x 1, :y 3, :z 3} {:x 4, :y 0, :z 0}
      {:x 0, :y 0, :z 0} {:x 4, :y 1, :z 4} {:x 0, :y 3, :z 0}
      {:x 5, :y 1, :z 5} {:x 1, :y 0, :z 0} {:x 2, :y 0, :z 0}
      {:x 5, :y 0, :z 0} {:x 0, :y 1, :z 0} {:x 0, :y 2, :z 0}
      {:x 2, :y 2, :z 4} {:x 3, :y 1, :z 3} {:x 1, :y 5, :z 5}}}))
   
  
  
