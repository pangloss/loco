(ns loco.constraints.arithmetic.multiplication-test
  (:require
   [clojure.test :refer :all]
   [loco.constraints :refer :all]
   [loco.constraints.test-utils :refer :all]
   ))

(deftest ^:loco multiplication-partial-test

  (testing "should compress and use offset for raw numbers"
    (test-loco
     [($in :x 0 5)
      ($= :x ($* 1 1))]
     {:model
      '[[:var :x :public [:int 0 5]]
        [arithm [:x = 1]]]}
     )

    (test-loco
     [($in :x 0 5)
      ($= :x ($* 1 2))]
     {:model
      '[[:var :x :public [:int 0 5]]
        [arithm [:x = 2]]]}
     )

    (test-loco
     [($in :x 0 5)
      ($= :x ($* 1 2 3 4))]
     {:model
      '[[:var :x :public [:int 0 5]]
        [arithm [:x = 24]]]}
     )

    (test-loco
     [($in :x 0 5)          
      ($= 24 ($* :x 1 2 3 4))]
     {:model
      '[[:var :x :public [:int 0 5]]
        [:view "x*24" [scale :x [24]] [:int 0 120]]
        [arithm [24 = "x*24"]]]}
     )

    (test-loco
     [($in :x 0 5)
      ($in :y 0 5)
      ($= 24 ($* :x :y 1 2 3 4))]
     {:model
      '[[:var :x :public [:int 0 5]]
        [:var :y :public [:int 0 5]]
        [:view "y*24" [scale :y [24]] [:int 0 120]]
        [:var "x*y*24" :proto [:int 0 600]]
        [times ["x*y*24" = :x * "y*24"]]
        [arithm [24 = "x*y*24"]]]}
     )

    (test-loco
     [($in :x 0 5)
      ($in :y 0 5)
      ($in :z 0 2)
      ($= 24 ($* :x :y :z 1 2 3 4))]
     {:model
      '[[:var :x :public [:int 0 5]]
        [:var :y :public [:int 0 5]]
        [:var :z :public [:int 0 2]]
        [:view "z*24" [scale :z [24]] [:int 0 48]]
        [:var "y*z*24" :proto [:int 0 240]]
        [:var "x*y*z*24" :proto [:int 0 1200]]
        [times ["y*z*24" = :y * "z*24"]]
        [times ["x*y*z*24" = :x * "y*z*24"]]
        [arithm [24 = "x*y*z*24"]]]
      :solutions #{{:x 1, :y 1, :z 1}}
      }
     )
    )

  (test-loco
   [($in :x 0 5)
    ($in :y 0 5)
    ($in :z 0 2)
    ($= 24 ($* :x :y :z))]
   {:model
    '[[:var :x :public [:int 0 5]]
      [:var :y :public [:int 0 5]]
      [:var :z :public [:int 0 2]]
      [:var "y*z" :proto [:int 0 10]]
      [:var "x*y*z" :proto [:int 0 50]]
      [times ["y*z" = :y * :z]]
      [times ["x*y*z" = :x * "y*z"]]
      [arithm [24 = "x*y*z"]]],
    :solutions #{{:x 4, :y 3, :z 2} {:x 3, :y 4, :z 2}}}
   )

  (testing "should handle 0 arity calls gracefully"
    (test-loco
     [($in :x 0 1)
      ($in :y -1 0)
      ($= :x ($* ))]
     {:model
      '[[:var :x :public [:bool 0 1]]
        [:var :y :public [:int -1 0]]]
      :solutions
      #{{:x 0, :y 0} {:x 1, :y 0} {:x 0, :y -1} {:x 1, :y -1}}
      }
     ))

  (testing "should handle 1 arity calls"
    (test-loco
     [($in :x 0 5)
      ($in :y -5 0)
      ($= :x ($* 1))]
     {:model
      '[[:var :x :public [:int 0 5]]
        [:var :y :public [:int -5 0]]
        [arithm [:x = 1]]],
      :compiled
      [["x = {0..5}" "y = {-5..0}"] ["ARITHM ([x = 1])"]],
      :solutions
      #{{:x 1, :y -5} {:x 1, :y 0} {:x 1, :y -3} {:x 1, :y -2}
        {:x 1, :y -1} {:x 1, :y -4}}})
    
    (test-loco
     [($in :x 0 5)
      ($in :y -5 0)
      ($= :x ($* :y))]
     {:model
      '[[:var :x :public [:int 0 5]]
        [:var :y :public [:int -5 0]]
        [arithm [:x = :y]]],
      :compiled
      [["x = {0..5}" "y = {-5..0}"] ["ARITHM ([prop(x.EQ.y)])"]],
      :solutions #{{:x 0, :y 0}}})
    
    )

  (test-loco
   [($in :g 1 2)
    ($in :h 1 2)
    ($in :j -1 2)
    ($= 0 ($* :g :h :j 1))]
   {:model
    '[[:var :g :public [:int 1 2]]
      [:var :h :public [:int 1 2]]
      [:var :j :public [:int -1 2]]
      [:var "h*j" :proto [:int -2 4]]
      [:var "g*h*j" :proto [:int -4 8]]
      [times ["h*j" = :h * :j]]
      [times ["g*h*j" = :g * "h*j"]]
      [arithm [0 = "g*h*j"]]],
    :compiled
    [["g = {1..2}"
      "h = {1..2}"
      "j = {-1..2}"
      "h*j = {-2..4}"
      "g*h*j = {-4..8}"]
     ["TABLE ([CSPLarge({h = {1..2}, , j = {-1..2}, , h*j = {-2..4}, })])"
      "TABLE ([CSPLarge({g = {1..2}, , h*j = {-2..4}, , g*h*j = {-4..8}, })])"
      "ARITHM ([g*h*j = 0])"]],
    :solutions
    #{{:g 1, :h 1, :j 0} {:g 2, :h 1, :j 0} {:g 1, :h 2, :j 0}
      {:g 2, :h 2, :j 0}}})

  (test-loco
   [($in :g 1 2)
    ($in :h 1 2)
    ($in :j -1 2)
    ($= 1 ($* :g :h :j 1 0))]
   {:model
    '[[:var :g :public [:int 1 2]]
      [:var :h :public [:int 1 2]]
      [:var :j :public [:int -1 2]]
      [arithm [1 = 0]]],
    :compiled
    [["g = {1..2}" "h = {1..2}" "j = {-1..2}"]
     ["ARITHM ([cste -- 1 = 0])"]],
    :solutions #{}})
  
  (test-loco
   [($in :x 1 2)
    ($in :y 1 2)
    ($in :z 0 2)
    ($= 0 ($* :y ($* :x :z)))]
   {:model
    '[[:var :x :public [:int 1 2]]
      [:var :y :public [:int 1 2]]
      [:var :z :public [:int 0 2]]
      [:var "x*z" :proto [:int 0 4]]
      [:var "y*x*z" :proto [:int 0 8]]
      [times ["x*z" = :x * :z]]
      [times ["y*x*z" = :y * "x*z"]]
      [arithm [0 = "y*x*z"]]],
    :compiled
    [["x = {1..2}"
      "y = {1..2}"
      "z = {0..2}"
      "x*z = {0..4}"
      "y*x*z = {0..8}"]
     ["TABLE ([CSPLarge({x = {1..2}, , z = {0..2}, , x*z = {0..4}, })])"
      "TABLE ([CSPLarge({y = {1..2}, , x*z = {0..4}, , y*x*z = {0..8}, })])"
      "ARITHM ([y*x*z = 0])"]],
    :solutions
    #{{:x 1, :y 1, :z 0} {:x 2, :y 1, :z 0} {:x 1, :y 2, :z 0}
      {:x 2, :y 2, :z 0}}}
   )

  (test-loco
   [($in :a 0 5)
    ($in :b 0 5)
    ($= ($minus-view :a) ($* :b))]
   {:model
    '[[:var :a :public [:int 0 5]]
      [:var :b :public [:int 0 5]]
      [:view "-a" [minus-view :a []] [:int -5 0]]
      [arithm ["-a" = :b]]],
    :compiled
    [["a = {0..5}" "b = {0..5}" "-(a = {0..5}) = [-5,0]"]
     ["ARITHM ([prop(-(a).EQ.b)])"]],
    :solutions #{{:a 0, :b 0}}}
   )
  )
