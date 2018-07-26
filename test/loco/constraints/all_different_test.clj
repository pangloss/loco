(ns loco.constraints.all-different-test
  (:require
   [clojure.test :refer :all]
   [loco.constraints :refer :all]
   [loco.constraints.test-utils :refer :all]
   ))

(deftest all-different-test
  (is
   (loco?
    [($in :x 0 1)
     ($in :y [1 4])
     ($in :z 1 2)
     ($distinct 0 :x :y :z)
     ]
    {:model
     '[[:var :x :public [:int 0 1]]
       [:var :y :public [:int [1 4]]]
       [:var :z :public [:int 1 2]]
       [all-different [0 :x :y :z]]],
     :compiled
     [["x = [0,1]" "y = {1,4}" "z = {1..2}"]
      ["ALLDIFFERENT ([PropAllDiffInst(cste -- 0, x, y, z), PropAllDiffBC(cste -- 0, x, y, z), PropAllDiffAdaptative(cste -- 0, x, y, z)])"]],
     :solutions #{{:x 1, :y 4, :z 2}}}
    ))

  (is
   (loco?
    [($in :x 0 1)
     ($in :y [1 4])
     ($in :z 1 2)
     ($distinct [0 :x :y :z] :ac)
     ($distinct [0 :x :y :z] :bc)
     ]
    {:model
     '[[:var :x :public [:int 0 1]]
       [:var :y :public [:int [1 4]]]
       [:var :z :public [:int 1 2]]
       [all-different [[0 :x :y :z] [consistency ac]]]
       [all-different [[0 :x :y :z] [consistency bc]]]],
     :compiled
     [["x = [0,1]" "y = {1,4}" "z = {1..2}"]
      ["ALLDIFFERENT ([PropAllDiffInst(cste -- 0, x, y, z), PropAllDiffAC(cste -- 0, x, y, z)])"
       "ALLDIFFERENT ([PropAllDiffInst(cste -- 0, x, y, z), PropAllDiffBC(cste -- 0, x, y, z)])"]],
     :solutions #{{:x 1, :y 4, :z 2}}}
    ))

  (is
   (loco?
    [($set :x [] [0 1 2])
     ($set :y [] [1 2])
     ($set :z [1 2])
     ($distinct [:x :y :z])
     ]
    {:model
     '[[:var :x :public [:set #{} #{0 1 2}]]
       [:var :y :public [:set #{} #{1 2}]]
       [:var :z :public [:set #{1 2}]]
       [all-different [:x :y :z]]],
     :compiled
     [["x = [{}, {0, 1, 2}]" "y = [{}, {1, 2}]" "z = {1, 2}"]
      ["SETALLDIFFERENT ([PropAllDiff(x, y, z), PropAllDiff(x, y, z), PropAtMost1Empty(x, y, z)])"]],
     :solutions
     #{{:x #{0 2}   , :y #{2}, :z #{1 2}}
       {:x #{0 1 2} , :y #{},  :z #{1 2}}
       {:x #{2}     , :y #{},  :z #{1 2}}
       {:x #{0 1}   , :y #{2}, :z #{1 2}}
       {:x #{0 1 2} , :y #{1}, :z #{1 2}}
       {:x #{0 1}   , :y #{1}, :z #{1 2}}
       {:x #{1}     , :y #{},  :z #{1 2}}
       {:x #{0}     , :y #{1}, :z #{1 2}}
       {:x #{0 1 2} , :y #{2}, :z #{1 2}}
       {:x #{0}     , :y #{2}, :z #{1 2}}
       {:x #{0 2}   , :y #{1}, :z #{1 2}}
       {:x #{2}     , :y #{1}, :z #{1 2}}
       {:x #{}      , :y #{1}, :z #{1 2}}
       {:x #{0}     , :y #{},  :z #{1 2}}
       {:x #{0 2}   , :y #{},  :z #{1 2}}
       {:x #{}      , :y #{2}, :z #{1 2}}
       {:x #{0 1}   , :y #{},  :z #{1 2}}
       {:x #{1}     , :y #{2}, :z #{1 2}}}}
    ))
  )
