(ns loco.constraints.arithm-test
  (:require
   [clojure.test :refer :all]
   [loco.constraints :refer :all]
   [loco.constraints.test-utils :refer :all]
   ))

(deftest arithm-test
  (is
   (loco?
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
    {:identity '[[:var :x :public [:int 0 100]]
                 [:var :y :public [:int 0 10]]
                 [:var :z :public [:int 0 5]]
                 [arithm [:y = :x]]
                 [arithm [:y != :x]]
                 [arithm [:y <= :x]]
                 [arithm [:y >= :x]]
                 [arithm [:y < :x]]
                 [arithm [:y > :x]]
                 [arithm [:y = :x * :z]]
                 [arithm [:y != :x + :z]]
                 [arithm [:y <= :x - :z]]
                 [arithm [:y >= :x / :z]]
                 [arithm [:y < :x + :z]]
                 [arithm [:y > :x + :z]]]
     :compiled [["x = {0..100}"
                 "y = {0..10}"
                 "z = {0..5}"]
                ["TIMES ([PropTimesNaive(x, z, IV_1)])"
                 "DIVISION ([PropDivXYZ(x, z, IV_2, ..., IV_2)])"
                 "ARITHM ([prop(y.EQ.x)])"
                 "ARITHM ([prop(y.NEQ.x)])"
                 "ARITHM ([prop(x.GEQ.y)])"
                 "ARITHM ([prop(y.GEQ.x)])"
                 "ARITHM ([x >= y + 1])"
                 "ARITHM ([y >= x + 1])"
                 "ARITHM ([prop(y.EQ.IV_1)])"
                 "SUM ([y - z - x != 0])"
                 "SUM ([z + y - x <= 0])"
                 "ARITHM ([prop(y.GEQ.IV_2)])"
                 "SUM ([y - z - x <= -1])"
                 "SUM ([y - z - x >= 1])"]]
     }))

  (is
   (loco?
    [($in :x 0 100)
     ($in :y 0 10)
     ($in :z 0 5)
     ($arithm :y := 5)
     ($arithm :y :!= 5)
     ($arithm :y :<= 5)
     ($arithm :y :>= 5)
     ($arithm :y :< 5)
     ($arithm :y :> 5)
     ($arithm :y := 5 :* :z)
     ($arithm :y :!= 5 :+ :z)
     ($arithm :y :<= 5 :- :z)
     ($arithm :y :>= 5 :/ :z)
     ($arithm :y :< 5 :+ :z)
     ($arithm :y :> 5 :+ :z)
     ]
    {
     :compiled
     [["x = {0..100}" "y = {0..10}" "z = {0..5}"]
      ["TIMES ([PropScale(z, IV_1)])"
       "DIVISION ([PropDivXYZ(cste -- 5, z, IV_2, ..., IV_2)])"
       "ARITHM ([y = 5])"
       "ARITHM ([y =/= 5])"
       "ARITHM ([y <= 5])"
       "ARITHM ([y >= 5])"
       "ARITHM ([y <= 4])"
       "ARITHM ([y >= 6])"
       "ARITHM ([prop(y.EQ.IV_1)])"
       "ARITHM ([prop(y.NEQ.z+5)])"
       "ARITHM ([z + y <= 5])"
       "ARITHM ([prop(y.GEQ.IV_2)])"
       "ARITHM ([z >= y + -4])"
       "ARITHM ([y >= z + 6])"]]
     })
   "constants are handled correctly")

  (is
   (loco?
    [($in :x 0 100)
     ($in :y 0 10)
     ($in :z 0 5)
     ($arithm 4 := :x)
     ($arithm 4 :!= :x)
     ($arithm 4 :<= :x)
     ($arithm 4 :>= :x)
     ($arithm 4 :< :x)
     ($arithm 4 :> :x)
     ($arithm 4 := :x :* :z)
     ($arithm 4 :!= :x :+ :z)
     ($arithm 4 :<= :x :- :z)
     ($arithm 4 :>= :x :/ :z)
     ($arithm 4 :< :x :+ :z)
     ($arithm 4 :> :x :+ :z)
     ]
    {
     :compiled
     [["x = {0..100}" "y = {0..10}" "z = {0..5}"]
      ["TIMES ([PropTimesNaive(x, z, IV_1)])"
       "DIVISION ([PropDivXYZ(x, z, IV_2, ..., IV_2)])"
       "ARITHM ([x = 4])"
       "ARITHM ([x =/= 4])"
       "ARITHM ([x >= 4])"
       "ARITHM ([x <= 4])"
       "ARITHM ([x >= 5])"
       "ARITHM ([x <= 3])"
       "ARITHM ([IV_1 = 4])"
       "ARITHM ([PropNotEqualXY_C(z, x)])"
       "ARITHM ([x >= z + 4])"
       "ARITHM ([IV_2 <= 4])"
       "ARITHM ([z + x >= 5])"
       "ARITHM ([z + x <= 3])"]]
     })
   "constants are handled correctly")
  
  
  (is
   (loco?
    [($in :x 0 100)
     ($in :y 0 10)
     ($in :z 0 5)
     ($arithm :y := :x :* 1)
     ($arithm :y :!= :x :+ 1)
     ($arithm :y :<= :x :- 1)
     ($arithm :y :>= :x :/ 1)
     ($arithm :y :< :x :+ 1)
     ($arithm :y :> :x :+ 1)
     ]
    {
     :compiled
     ;;Choco is doing smart stuff here, so the output is a little hard to follow
     [["x = {0..100}" "y = {0..10}" "z = {0..5}"]
      ["ARITHM ([prop(x.EQ.IV_1)])"
       "DIVISION ([PropDivXYZ(x, cste -- 1, IV_2, ..., IV_2)])"
       "ARITHM ([prop(y.EQ.IV_1)])"
       "ARITHM ([prop(y.NEQ.x+1)])"
       "ARITHM ([x >= y + 1])"
       "ARITHM ([prop(y.GEQ.IV_2)])"
       "ARITHM ([prop(x.GEQ.y)])"
       "ARITHM ([y >= x + 2])"]]
     })
   "constants are handled correctly")

  (is
   (loco?
    [($in :x 0 100)
     ($in :y 0 10)
     ($arithm :y := :x)
     ]
    {:solutions
     #{{:x 0, :y 0} {:x 10, :y 10} {:x 7, :y 7} {:x 1, :y 1}
       {:x 9, :y 9} {:x 2, :y 2} {:x 4, :y 4} {:x 3, :y 3}
       {:x 8, :y 8} {:x 6, :y 6} {:x 5, :y 5}}}))

  (is
   (loco?
    [($in :x 0 3)
     ($in :y 0 3)
     ($arithm :y :!= :x)
     ]
    {:solutions
     #{{:x 2, :y 1} {:x 1, :y 0} {:x 3, :y 2} {:x 3, :y 1}
       {:x 3, :y 0} {:x 2, :y 0} {:x 2, :y 3} {:x 0, :y 2}
       {:x 1, :y 2} {:x 0, :y 3} {:x 0, :y 1} {:x 1, :y 3}}}))

  (is
   (loco?
    [($in :x 0 3)
     ($in :y 0 3)
     ($arithm :y :<= :x)
     ]
    {:solutions
     #{{:x 0, :y 0} {:x 2, :y 1} {:x 1, :y 0} {:x 3, :y 2}
       {:x 3, :y 1} {:x 3, :y 0} {:x 2, :y 0} {:x 1, :y 1}
       {:x 2, :y 2} {:x 3, :y 3}}}))

  (is
   (loco?
    [($in :x 0 2)
     ($in :y 0 2)
     ($arithm :y :>= :x)
     ]
    {:solutions
     #{{:x 0, :y 0} {:x 1, :y 1} {:x 0, :y 2} {:x 1, :y 2}
       {:x 2, :y 2} {:x 0, :y 1}}}))

  (is
   (loco?
    [($in :x 0 2)
     ($in :y 0 2)
     ($arithm :y :< :x)
     ]
    {:solutions
     #{{:x 2, :y 1} {:x 1, :y 0} {:x 2, :y 0}}}))

  (is
   (loco?
    [($in :x 0 2)
     ($in :y 0 2)
     ($arithm :y :> :x)
     ]
    {:solutions
     #{{:x 0, :y 2} {:x 1, :y 2} {:x 0, :y 1}}}))


  (is
   (loco?
    [($in :x [5 10])
     ($in :y 0 100)
     ($in :z [0 5])
     ($arithm :y := :x :* :z)]
    {:solutions
     #{{:x 10, :y 0, :z 0} {:x 5, :y 25, :z 5} {:x 5, :y 0, :z 0}
       {:x 10, :y 50, :z 5}}}))

  (is
   (loco?
    [($in :x [0 1])
     ($in :y 1 1)
     ($in :z [0 3])
     ($arithm :y :!= :x :+ :z)]
    {:solutions
     #{{:x 1, :y 1, :z 3} {:x 0, :y 1, :z 3} {:x 0, :y 1, :z 0}}}))

  (is
   (loco?
    [($in :x 0 100)
     ($in :y 99 100)
     ($in :z 0 5)
     ($arithm :y :<= :x :- :z)]
    {:solutions
     #{{:x 100, :y 99, :z 0} {:x 100, :y 100, :z 0}
       {:x 99, :y 99, :z 0} {:x 100, :y 99, :z 1}}}))

  (is
   (loco?
    [($in :x 0 100)
     ($in :y 2 4)
     ($in :z 0 5)
     ($arithm :y := :x :* :z)]
    {:solutions
     #{{:x 1, :y 4, :z 4} {:x 1, :y 2, :z 2} {:x 1, :y 3, :z 3}
       {:x 2, :y 2, :z 1} {:x 3, :y 3, :z 1} {:x 2, :y 4, :z 2}
       {:x 4, :y 4, :z 1}}}))

  (is
   (loco?
    [($in :x 0 100)
     ($in :y 2 4)
     ($in :z 0 2)
     ($arithm :y := :x :/ :z)]
    {:solutions
     #{
       {:x 5, :y 2, :z 2} ;;div rounding :( TODO: add documentation or talk to the Choco dudes about this
       {:x 7, :y 3, :z 2} ;;div rounding
       {:x 9, :y 4, :z 2} ;;div rounding
       {:x 8, :y 4, :z 2} {:x 4, :y 2, :z 2} {:x 2, :y 2, :z 1}
       {:x 3, :y 3, :z 1} {:x 6, :y 3, :z 2} {:x 4, :y 4, :z 1}
       }}))
  )
