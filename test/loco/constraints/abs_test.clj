(ns loco.constraints.abs-test
  (:require
   [loco.model :as model]
   [loco.compiler :as compiler]
   [loco.solver :as solver]
   [loco.constraints.test-utils :refer :all])
  (:use
   loco.constraints
   clojure.test))

;;FIXME: remake these as (is (test-loco)) form

#_(deftest ^:model abs-model-test
  (are [expected input] (= expected (->> input model/compile))
    [[:var :y :public [:int 0 10]]
     [:var :z :public [:int -5 0]]
     [:constraint ['abs [:y '= :z]]]]
    [($in :y 0 10)
     ($in :z -5 0)
     ($abs :y :z)]

    [[:var :y :public [:int 0 10]]
     [:var :z :public [:int -5 0]]
     [:var :|z| :proto [:int 0 5]]
     [:constraint ['abs [:|z| '= :z]]]
     [:constraint ['arithm [:y '= :|z|]]]]
    [($in :y 0 10)
     ($in :z -5 0)
     ($= :y ($abs :z))]

    [[:var :x :public [:int -5 5]]
     [:var :|x| :proto [:int 0 5]]
     [:constraint ['abs [:|x| '= :x]]]
     [:constraint ['arithm [:|x| '= 2]]]]
    [($in :x -5 5)
     ($= ($abs :x) 2)]

    )
  )

#_(deftest ^:compiler abs-compile-test
  (are [expected input] (= expected (utils/constraints-strings input))
    '("ABSOLUTE ([y = {0..10} = |z = {-5..0}|])")
    [($in :y 0 10)
     ($in :z -5 0)
     ($abs :y :z)]

    '("ABSOLUTE ([|x| = {0..5} = |x = {-5..5}|])"
      "ARITHM ([|x| = 2])")
    [($in :x -5 5)
     ($= ($abs :x) 2)]
    )
  )

#_(deftest ^:solutions abs-solution-test
  (are [expected input] (= expected (solver/solutions input))
    '({:x 5, :y -5}
      {:x 4, :y -4}
      {:x 1, :y -1}
      {:x 3, :y -3}
      {:x 2, :y -2})
    [($in :x 1 5)
     ($in :y -5 -1)
     ($abs :x :y)]
    )
  )
