(ns loco.constraints.arithm-test
  (:require
   [loco.model :as model]
   [loco.compiler :as compiler]
   [loco.solver :as solver]
   [loco.constraints.test-utils :as utils])
  (:use
   loco.constraints
   clojure.test))

(deftest ^:model arithm-model-test
  (are [expected input] (= expected (->> input model/compile))
    [[:var :x :public [:int 0 100]]
     [:var :y :public [:int 0 10]]
     [:var :z :public [:int 0 5]]
     [:constraint [:con/arithm [:y :op/= :x]]]
     [:constraint [:con/arithm [:y :op/!= :x]]]
     [:constraint [:con/arithm [:y :op/<= :x]]]
     [:constraint [:con/arithm [:y :op/>= :x]]]
     [:constraint [:con/arithm [:y :op/< :x]]]
     [:constraint [:con/arithm [:y :op/> :x]]]
     [:constraint [:con/arithm [:y :op/= :x :op/* :z]]]
     [:constraint [:con/arithm [:y :op/!= :x :op/+ :z]]]
     [:constraint [:con/arithm [:y :op/<= :x :op/- :z]]]
     [:constraint [:con/arithm [:y :op/>= :x :op// :z]]]
     [:constraint [:con/arithm [:y :op/< :x :op/+ :z]]]
     [:constraint [:con/arithm [:y :op/> :x :op/+ :z]]]]
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
     [:constraint [:con/arithm [:y :op/= :x :op// 1]]]
     [:constraint [:con/arithm [:y :op/!= :x :op// 2]]]]
    [($in :x 0 100)
     ($in :y 0 10)
     ($arithm :y := :x :/ 1)
     ($arithm :y :!= :x :/ 2)]

    [[:var :x :public [:int 1 5]]
     [:var :y :public [:int 1 5]]
     [:var :z :public [:int 1 5]]
     [:var :a :public [:int 1 5]]
     [:var :b :public [:int 1 5]]
     [:constraint [:con/arithm [:z :op/= 2]]]
     [:constraint [:con/arithm [:x :op/< :y]]]
     [:constraint [:con/arithm [:y :op/<= :z]]]
     [:constraint [:con/arithm [:y :op/> :x]]]
     [:constraint [:con/arithm [:z :op/>= :y]]]
     [:constraint [:con/arithm [:x :op/!= :y]]]]
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
  (are [expected input] (= expected (utils/constraints-strings input))
    '("DIVISION ([PropDivXYZ(x, cste -- 10, IV_1, ..., IV_1)])"
      "ARITHM ([prop(y.EQ.IV_1)])")
    [($in :x 0 100)
     ($in :y 0 10)
     ($arithm :y := :x :/ 10)]

    '("ARITHM ([prop(y.EQ.x)])")
    [($in :x 0 100)
     ($in :y 0 10)
     ($arithm :y := :x)]
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
     ($arithm :y := :x :/ 10)]

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
