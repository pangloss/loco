(ns loco.constraints.views.abs-test
  (:require
   [clojure.test :refer :all]
   [loco.constraints :refer :all]
   [loco.constraints.test-utils :refer :all]
   ))

(deftest abs-view-test
  (test-loco
   [($in :a -5 5)
    ($= :a ($abs-view 3))
    ]
   {:identity '[[:var :a :public [:int -5 5]]
                [arithm [:a = 3]]],
    :model '[[:var :a :public [:int -5 5]]
             [arithm [:a = 3]]],
    :compiled [["a = {-5..5}"]
               ["ARITHM ([a = 3])"]],
    :solutions #{{:a 3}}})
  

  (test-loco
   [($in :a -5 5)
    ($= :a ($abs-view :a))
    ]
   {:identity
    '[[:var :a :public [:int -5 5]]
      [arithm [:a = [abs :a []]]]],
    :model
    '[[:var :a :public [:int -5 5]]
      [:view "|a|" [abs :a []] [:int 5 5]]
      [arithm [:a = "|a|"]]],
    :compiled
    [["a = {-5..5}"
      "|a| = {0..5}"]
     ["ABSOLUTE ([|a| = {0..5} = |a = {-5..5}|])"
      "ARITHM ([prop(a.EQ.|a|)])"]],
    :solutions #{{:a 1} {:a 0} {:a 3} {:a 4} {:a 2} {:a 5}}})
  
  
  )
