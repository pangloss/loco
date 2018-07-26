(ns loco.constraints.all-equal-test
  (:use
   loco.constraints
   clojure.test)
  (:require
   [loco.model :as model]
   [loco.compiler :as compiler]
   [loco.solver :as solver]
   [loco.constraints.test-utils :as utils])
 )

(def fib [1 2 3 5 8 13])

(deftest ^:model model-test
  (are [expected input] (= expected (->> input model/compile))
    [
     [:var :z :public [:int 1 5]]
     [:var :a :public [:int 1 5]]
     [:var :b :public [:int 1 5]]
     [:var :2 :hidden [:const 2]]
     [:constraint ['all-equal [:z :2 :a :b]]]]
    [
     ($in :z 1 5)
     ($in :a 1 5)
     ($in :b 1 5)
     ($= :z 2 :a :b)]
    )
  )

#_(deftest ^:compiler compile-test
  (are [expected input] (= expected (utils/constraints-strings input))
    '("ATMOSTNVALUES ([PropAtMostNValues(x, y, 1, cste -- 1)])")
    [($in :x 0 5)
     ($in :y 0 2)
     ($= :x :y 1)]

    '("ATMOSTNVALUES ([PropAtMostNValues(x, y, cste -- 1)])")
    [($in :x 0 5)
     ($in :y 0 2)
     ($all-equal [:x :y])]

    '("SETALLEQUAL ([PropAllEqual(x, y)])")
    [($set :x [0 5] [0 5 6 7 8 9])
     ($set :y [0 2] [0 2 5 3 4])
     ($all-equal [:x :y])]
    )
  )

(deftest ^:solutions solution-test
  (are [expected input] (= expected (solver/solutions input))
    '({:x 1, :y 1, :z 1}
      {:x 2, :y 2, :z 2}
      {:x 3, :y 3, :z 3}
      {:x 4, :y 4, :z 4}
      {:x 5, :y 5, :z 5})
    [($in :x 1 5)
     ($in :y 1 5)
     ($in :z 1 5)
     ($= :z :x :y)]

    '({:x #{1 5}, :y #{1 5}, :z #{1 5}}
      {:x #{1}, :y #{1}, :z #{1}})
    [($set :x [1] [1 5])
     ($set :y [] [1 5])
     ($set :z [] [0 1 5])
     ($= :z :x :y)]
    )
  )
