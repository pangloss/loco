(ns loco.constraints.views.affine-test
  (:require
   [clojure.test :refer :all]
   [loco.constraints :refer :all]
   [loco.constraints.test-utils :refer :all]
   ))

(deftest affine-test
  (test-loco
   [($in :a -5 5)
    ($in :b 0 100)
    ($= :b ($affine :a 1 3))
    ]
   {:model
    '[[:var :a :public [:int -5 5]]
      [:var :b :public [:int 0 100]]
      [:view "a+3" [affine :a [1 3]] [:int -5 5]]
      [arithm [:b = "a+3"]]],
    :compiled
    [["a = {-5..5}" "b = {0..100}" "(a = {-5..5} + 3) = [-2,8]"]
     ["ARITHM ([prop(b.EQ.(a+3))])"]],
    :solutions
    #{{:a -3, :b 0} {:a 5, :b 8} {:a 2, :b 5} {:a 4, :b 7}
      {:a 0, :b 3} {:a -1, :b 2} {:a 1, :b 4} {:a -2, :b 1}
      {:a 3, :b 6}}})

  (test-loco
   [($in :a -5 5)
    ($in :b 0 100)
    ($= :b ($affine :a -1 3))
    ]
   {:model
    '[[:var :a :public [:int -5 5]]
      [:var :b :public [:int 0 100]]
      [:view "-(a)+3" [affine :a [-1 3]] [:int -5 5]]
      [arithm [:b = "-(a)+3"]]],
    :compiled
    [["a = {-5..5}"
      "b = {0..100}"
      "(-(a = {-5..5}) = [-5,5] + 3) = [-2,8]"]
     ["ARITHM ([prop(b.EQ.(-(a)+3))])"]],
    :solutions
    #{{:a -3, :b 6} {:a -1, :b 4} {:a 1, :b 2} {:a -4, :b 7}
      {:a -5, :b 8} {:a 2, :b 1} {:a 3, :b 0} {:a 0, :b 3}
      {:a -2, :b 5}}})

  (test-loco
   [($in :a -5 5)
    ($in :b 0 100)
    ($= :b ($affine :a 0 3))
    ]
   {:model
    '[[:var :a :public [:int -5 5]]
      [:var :b :public [:int 0 100]]
      [:view "3" [affine :a [0 3]] [:int 0 0]]
      [arithm [:b = "3"]]],
    :compiled
    [["a = {-5..5}" "b = {0..100}" "(cste -- 0+3) = 3"]
     ["ARITHM ([b = 3])"]],
    :solutions
    #{{:a -2, :b 3} {:a 4, :b 3} {:a -5, :b 3} {:a -1, :b 3}
      {:a 0, :b 3} {:a 2, :b 3} {:a 3, :b 3} {:a -4, :b 3}
      {:a 1, :b 3} {:a 5, :b 3} {:a -3, :b 3}}})


  (test-loco
   [($in :a -5 5)
    ($in :b 0 100)
    ($= :b ($affine :a 2 3))
    ]
   {:model
    '[[:var :a :public [:int -5 5]]
      [:var :b :public [:int 0 100]]
      [:view "2a+3" [affine :a [2 3]] [:int -10 10]]
      [arithm [:b = "2a+3"]]],
    :compiled
    [["a = {-5..5}"
      "b = {0..100}"
      "((a = {-5..5} * 2) = [-10,10] + 3) = [-7,13]"]
     ["ARITHM ([prop(b.EQ.((a*2)+3))])"]],
    :solutions
    #{{:a 1, :b 5} {:a -1, :b 1} {:a 3, :b 9} {:a 4, :b 11}
      {:a 5, :b 13} {:a 0, :b 3} {:a 2, :b 7}}})

  (test-loco
   [($in :a -5 5)
    ($in :b 0 100)
    ($= :b ($affine 2 2 3))
    ]
   {:model
    '[[:var :a :public [:int -5 5]]
      [:var :b :public [:int 0 100]]
      [:view "7" [affine 2 [2 3]] [:int 4 4]]
      [arithm [:b = "7"]]],
    :compiled
    [["a = {-5..5}" "b = {0..100}" "cste -- 7 = 7"]
     ["ARITHM ([b = 7])"]],
    :solutions
    #{{:a 5, :b 7} {:a 3, :b 7} {:a 0, :b 7} {:a -4, :b 7}
      {:a 4, :b 7} {:a -1, :b 7} {:a -5, :b 7} {:a -3, :b 7}
      {:a -2, :b 7} {:a 1, :b 7} {:a 2, :b 7}}})

  )
