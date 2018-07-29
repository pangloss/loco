(ns loco.constraints.div-test
  (:require
   [clojure.test :refer :all]
   [loco.constraints :refer :all]
   [loco.constraints.test-utils :as utils]
   ))

(deftest div-test
  (is
   (loco?
    [($in :x 0 2)
     ($in :y 0 2)
     ($in :z 0 2)
     ($div :z :x :y)]
    {:model
     '[[:var :x :public [:int 0 2]]
       [:var :y :public [:int 0 2]]
       [:var :z :public [:int 0 2]]
       [div [:z = :x / :y]]],
     :compiled
     [["x = {0..2}" "y = {0..2}" "z = {0..2}"]
      ["DIVISION ([PropDivXYZ(x, y, z, ..., z)])"]],
     :solutions
     #{{:x 1, :y 1, :z 1} {:x 2, :y 1, :z 2} {:x 2, :y 2, :z 1}
       {:x 1, :y 2, :z 0} {:x 0, :y 1, :z 0} {:x 0, :y 2, :z 0}}}
    ))

  (is (thrown? AssertionError ($div 0 0 0)))
  )
