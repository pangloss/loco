(ns loco.constraints.times-test
  (:require
   [loco.model :as model]
   [loco.compiler :as compiler]
   [loco.solver :as solver]
   [loco.constraints.test-utils :as utils])
  (:use
   loco.constraints
   clojure.test))

(deftest ^:model times-model-test
  (are [expected input] (= expected (->> input model/compile))
    [[:var :x :public [:int 0 100]]
     [:var :y :public [:int 0 10]]
     [:var :z :public [:int 0 5]]
     [:constraint ['times [:z '= :x '* :y]]]]
    [($in :x 0 100)
     ($in :y 0 10)
     ($in :z 0 5)
     ($times :z :x :y)]

    [[:var :x :public [:int 0 100]]
     [:var :y :public [:int 0 10]]
     [:var :z :public [:int 0 5]]
     [:constraint ['times [:z '= :x '* :y]]]]
    [($in :x 0 100)
     ($in :y 0 10)
     ($in :z 0 5)
     ($times :z = :x * :y)]
    )
  )

(deftest ^:compiler times-compile-test
  (are [expected input] (= expected (utils/constraints-strings input))
    '("TABLE ([CSPLarge({x = {0..100}, , y = {0..10}, , z = {0..5}, })])")
    [($in :x 0 100)
     ($in :y 0 10)
     ($in :z 0 5)
     ($times :z :x :y)]
    )
  )

(deftest ^:solutions times-solution-test
  (are [expected input] (= expected (solver/solutions input))
    '({:x 1, :y 1, :z 1}
      {:x 1, :y 2, :z 2}
      {:x 1, :y 3, :z 3}
      {:x 1, :y 4, :z 4}
      {:x 1, :y 5, :z 5}
      {:x 2, :y 1, :z 2}
      {:x 3, :y 1, :z 3}
      {:x 4, :y 1, :z 4}
      {:x 5, :y 1, :z 5}
      {:x 2, :y 2, :z 4})
    [($in :x 1 100)
     ($in :y 1 10)
     ($in :z 1 5)
     ($times :z :x :y)]
    )
  )
