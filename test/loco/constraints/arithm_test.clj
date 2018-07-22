(ns loco.constraints.arithm-test
  (:require
   [clojure.test :refer :all]
   [loco.compiler :as compiler]
   [loco.constraints :refer :all]
   [loco.constraints.test-utils :refer :all]
   [loco.model :as model]
   [loco.solver :as solver]
   ))

(deftest ^:model arithm-model-test
  (are [expected input] (= expected (->> input model/compile))
    [[:var :x :public [:int 0 100]]
     [:var :y :public [:int 0 10]]
     [:var :z :public [:int 0 5]]
     [:constraint ['arithm [:y '= :x]]]
     [:constraint ['arithm [:y '!= :x]]]
     [:constraint ['arithm [:y '<= :x]]]
     [:constraint ['arithm [:y '>= :x]]]
     [:constraint ['arithm [:y '< :x]]]
     [:constraint ['arithm [:y '> :x]]]
     [:constraint ['arithm [:y '= :x '* :z]]]
     [:constraint ['arithm [:y '!= :x '+ :z]]]
     [:constraint ['arithm [:y '<= :x '- :z]]]
     [:constraint ['arithm [:y '>= :x '/ :z]]]
     [:constraint ['arithm [:y '< :x '+ :z]]]
     [:constraint ['arithm [:y '> :x '+ :z]]]]
    [($in :x 0 100)
     ($in :y 0 10)
     ($in :z 0 5)
     ($arithm :y := :x)
     ($arithm :y :!= :x)
     ($arithm :y :<= :x)
     ($arithm :y :>= :x)
     ($arithm :y :< :x)
     ($arithm :y :> :x)
     ($arithm :y := :x :* :z)
     ($arithm :y :!= :x :+ :z)
     ($arithm :y :<= :x :- :z)
     ($arithm :y :>= :x :/ :z)
     ($arithm :y :< :x :+ :z)
     ($arithm :y :> :x :+ :z)]

    [[:var :x :public [:int 0 100]]
     [:var :y :public [:int 0 10]]
     [:constraint ['arithm [:y '= :x '/ 1]]]
     [:constraint ['arithm [:y '!= :x '/ 2]]]]
    [($in :x 0 100)
     ($in :y 0 10)
     ($arithm :y := :x :/ 1)
     ($arithm :y :!= :x :/ 2)]

    [[:var :x :public [:int 1 5]]
     [:var :y :public [:int 1 5]]
     [:var :z :public [:int 1 5]]
     [:var :a :public [:int 1 5]]
     [:var :b :public [:int 1 5]]
     [:constraint ['arithm [:z '= 2]]]
     [:constraint ['arithm [:x '< :y]]]
     [:constraint ['arithm [:y '<= :z]]]
     [:constraint ['arithm [:y '> :x]]]
     [:constraint ['arithm [:z '>= :y]]]
     [:constraint ['arithm [:x '!= :y]]]]
    [($in :x 1 5)
     ($in :y 1 5)
     ($in :z 1 5)
     ($in :a 1 5)
     ($in :b 1 5)
     ($= :z 2)
     ($< :x :y)
     ($<= :y :z)
     ($> :y :x)
     ($>= :z :y)
     ($!= :x :y)]
    )
  )

(deftest ^:compiler arithm-compile-test
  (are [expected input] (= expected (compiled-constraints-strings input))
    '("DIVISION ([PropDivXYZ(x, cste -- 10, IV_1, ..., IV_1)])"
      "ARITHM ([prop(y.EQ.IV_1)])")
    [($in :x 0 100)
     ($in :y 0 10)
     ($arithm :y := :x :/ 10)]

    '("ARITHM ([prop(y.EQ.x)])")
    [($in :x 0 100)
     ($in :y 0 10)
     ($arithm :y := :x)]

    '("ARITHM ([y = {0..10} + x = {0..100} = 0])")
    [($in :x 0 100)
     ($in :y 0 10)
     ($arithm 0 := :x + :y)]

    '("ARITHM ([prop(cste -- 6.EQ.cste -- 5+1)])")
    [($arithm 6 := 5 + 1)]
    )
  )

(deftest ^:solutions arithm-solution-test
  (are [expected input] (= expected (solver/solutions input))
    '({:x 0, :y 0}
      {:x 2, :y 0}
      {:x 5, :y 0}
      {:x 10, :y 1}
      {:x 11, :y 1})
    [($in :x [0 2 5 10 11])
     ($in :y 0 10)
     ($arithm :y = :x / 10)]

    '({:x 0, :y 0}
      {:x 1, :y 1}
      {:x 2, :y 2}
      {:x 3, :y 3}
      {:x 4, :y 4}
      {:x 5, :y 5})
    [($in :x 0 5)
     ($in :y 0 5)
     ($arithm :y := :x)]
    )
  )
