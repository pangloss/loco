(ns loco.constraints.all-different-except-0-test
  (:require
   [clojure.test :refer :all]
   [loco.constraints :refer :all]
   [loco.constraints.test-utils :refer :all]
   ))

(deftest all-different-except-0-test
  (test-loco
   [($in :x 0 1)
    ($in :y [1 4])
    ($in :z 1 2)
    ($distinct-except-0 0 :x :y :z)
    ]
   {:model
    '[[:var :x :public [:bool 0 1]]
      [:var :y :public [:int [1 4]]]
      [:var :z :public [:int 1 2]]
      [all-different-except-0 [0 :x :y :z]]],
    :compiled
    [["x = [0,1]" "y = {1,4}" "z = {1..2}"]
     ["ALLDIFFERENT ([PropCondAllDiffInst(cste -- 0, x, y, z), PropCondAllDiff_AC(cste -- 0, x, y, z)])"]],
    :solutions
    #{{:x 1, :y 4, :z 2}
      {:x 0, :y 4, :z 2}
      {:x 0, :y 1, :z 2}
      {:x 0, :y 4, :z 1}}}
   )
  )
