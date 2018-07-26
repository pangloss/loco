(ns loco.constraints.not-all-equal-test
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
     [:constraint ['not-all-equal [:z :2 :a :b]]]]
    [
     ($in :z 1 5)
     ($in :a 1 5)
     ($in :b 1 5)
     ($!= :z 2 :a :b)]
    )
  )

#_(deftest ^:compiler compile-test
  (are [expected input] (= expected (utils/constraints-strings input))
    '("ATLEASTNVALUES ([PropAtLeastNValues(x, y, cste -- 2)])")
    [($in :x 0 5)
     ($in :y 0 2)
     ($not-all-equal [:x :y])]

    '("ATLEASTNVALUES ([PropAtLeastNValues(x, y, 1, cste -- 2)])")
    [($in :x 0 5)
     ($in :y 0 2)
     ($!= :x :y 1)]
    )
  )

(deftest ^:solutions solution-test
  (are [expected input] (= expected (solver/solutions input))
    '({:x 2, :y 1, :z 1}
      {:x 3, :y 1, :z 1}
      {:x 4, :y 1, :z 1}
      {:x 5, :y 1, :z 1}
      {:x 1, :y 1, :z 2}
      {:x 2, :y 1, :z 2}
      {:x 3, :y 1, :z 2}
      {:x 4, :y 1, :z 2}
      {:x 5, :y 1, :z 2})
    [($in :x 1 5)
     ($in :y 1 1)
     ($in :z 1 2)
     ($!= :z :x :y)]
    )
  )
