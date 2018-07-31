(ns loco.constraints.distance-test
  (:require
   [clojure.test :refer :all]
   [loco.constraints :refer :all]
   [loco.constraints.test-utils :refer :all]
   ))

(deftest distance-test
  (is
   (loco?
    [($in :x 0 3)
     ($in :y 0 3)
     ($in :eq 0 3)

     ($distance :x :y = :eq)]
    {:model
     '[[:var :x :public [:int 0 3]]
       [:var :y :public [:int 0 3]]
       [:var :eq :public [:int 0 3]]
       [distance [| :x - :y | = :eq]]],
     :compiled
     [["x = {0..3}" "y = {0..3}" "eq = {0..3}"]
      ["DISTANCE ([|x = {0..3} - y = {0..3}| = eq = {0..3}])"]],
     :solutions
     #{{:x 1, :y 1, :eq 0} {:x 1, :y 2, :eq 1}
       {:x 0, :y 2, :eq 2} {:x 1, :y 0, :eq 1}
       {:x 3, :y 3, :eq 0} {:x 0, :y 1, :eq 1}
       {:x 3, :y 2, :eq 1} {:x 3, :y 1, :eq 2}
       {:x 2, :y 3, :eq 1} {:x 0, :y 0, :eq 0}
       {:x 0, :y 3, :eq 3} {:x 3, :y 0, :eq 3}
       {:x 2, :y 0, :eq 2} {:x 2, :y 1, :eq 1}
       {:x 2, :y 2, :eq 0} {:x 1, :y 3, :eq 2}}}
    ))

  (is
   (loco?
    [($in :eq 0 3)
     ($distance 3 :eq not= 1)]
    {:compiled
     [["eq = {0..3}"] ["DISTANCE ([|cste -- 3 - eq|=/=1])"]],
     :solutions #{{:eq 1} {:eq 3} {:eq 0}}}
    ))

  (is
   (loco?
    [($distance 3 3 not= 1)]
    {:compiled [[] ["DISTANCE ([|cste -- 3 - cste -- 3|=/=1])"]]}
    ))

  )
