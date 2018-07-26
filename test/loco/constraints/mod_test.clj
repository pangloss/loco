(ns loco.constraints.mod-test
  (:require
   [loco.model :as model]
   [loco.compiler :as compiler]
   [loco.solver :as solver]
   [loco.constraints.test-utils :as utils])
  (:use
   loco.constraints
   clojure.test))

(deftest ^:model mod-model-test
  (are [expected input] (= expected (->> input model/compile))
    [[:var :x :public [:int 0 100]]
     [:var :y :public [:int 0 10]]
     [:var :z :public [:int 0 5]]
     [:constraint ['mod [:z '= :x '% :y]]]]
    [($in :x 0 100)
     ($in :y 0 10)
     ($in :z 0 5)
     ($mod :z :x :y)]

    [[:var :x :public [:int 0 100]]
     [:var :y :public [:int 0 10]]
     [:var :z :public [:int 0 5]]
     [:constraint ['mod [:z '= :x '% :y]]]]
    [($in :x 0 100)
     ($in :y 0 10)
     ($in :z 0 5)
     ($mod :z = :x '% :y)]

    [[:var :x :public [:int 0 100]]
     [:var :y :public [:int 0 10]]
     [:var :z :public [:int 0 5]]
     [:var :x%y :proto [:int 0 10]]
     [:constraint ['mod [:x%y '= :x '% :y]]]
     [:constraint ['arithm [:z '= :x%y]]]]
    [($in :x 0 100)
     ($in :y 0 10)
     ($in :z 0 5)
     ($= :z ($mod :x :y))]
    )
  )

#_(deftest ^:compiler mod-compile-test
  (are [expected input] (= expected (utils/constraints-strings input))
    '("ABSOLUTE ([|T1_1| = [0,100] = |T1_1 = [-100,100]|])"
      "DIVISION ([PropDivXYZ(x, y, T1_1, ..., |T1_1|)])"
      "TIMES ([PropTimesNaive(T1_1, y, T2_2)])"
      "SUM ([PropXplusYeqZ(z, T2_2, x)])")
    [($in :x 0 100)
     ($in :y 0 10)
     ($in :z 0 5)
     ($mod :z :x :y)]
    )
  )

(deftest ^:solutions mod-solution-test
  (are [expected input] (= expected (solver/solutions input))
    '({:x 1, :y 2, :z 1}
      {:x 1, :y 3, :z 1}
      {:x 1, :y 4, :z 1}
      {:x 1, :y 5, :z 1}
      {:x 3, :y 2, :z 1}
      {:x 4, :y 3, :z 1}
      {:x 5, :y 2, :z 1}
      {:x 5, :y 4, :z 1}
      {:x 2, :y 3, :z 2}
      {:x 2, :y 4, :z 2}
      {:x 2, :y 5, :z 2}
      {:x 3, :y 4, :z 3}
      {:x 3, :y 5, :z 3}
      {:x 4, :y 5, :z 4}
      {:x 5, :y 3, :z 2})
    [($in :x 1 5)
     ($in :y 1 5)
     ($in :z 1 5)
     ($mod :z :x :y)]
    )
  )
