(ns loco.constraints.all-equal-test
  (:require
   [clojure.test :refer :all]
   [loco.constraints :refer :all]
   [loco.constraints.test-utils :refer :all]
   )
  )

(deftest all-equal-test
  (is
   (loco?
    [($= 1 1 1 1)]
    {
     :model '[[all-equal [1 1 1 1]]],
     :compiled [[]
                ["ATMOSTNVALUES ([PropAtMostNValues(cste -- 1, cste -- 1, cste -- 1, ..., cste -- 1)])"]],
     :solutions #{{}}}
    ))

  (is
   (loco?
    [
     ($in :z 1 5)
     ($in :a 1 5)
     ($in :b 1 5)
     ($= :z 2)
     ($= [:z 2 :a :b])
     ($= :z 2 :a :b)]
    {:model
     '[[:var :z :public [:int 1 5]]
       [:var :a :public [:int 1 5]]
       [:var :b :public [:int 1 5]]
       [arithm [:z = 2]]
       [all-equal [:z 2 :a :b]]],
     :compiled
     [["z = {1..5}" "a = {1..5}" "b = {1..5}"]
      ["ARITHM ([z = 2])"
       "ATMOSTNVALUES ([PropAtMostNValues(z, cste -- 2, a, ..., cste -- 1)])"]],
     :solutions #{{:z 2, :a 2, :b 2}}}
    ))

  (is
   (loco?
    [($set :x [0 5] [0 2 5 6 7 8 9])
     ($set :y [0 2] [0 2 5 3 4])
     ($all-equal [:x :y])]
    {
     :model
     '[[:var :x :public [:set #{0 5} #{0 2 5 6 7 8 9}]]
       [:var :y :public [:set #{0 2} #{0 2 3 4 5}]]
       [all-equal [:x :y]]],
     :compiled
     [["x = [{0, 5}, {0, 2, 5, 6, 7, 8, 9}]"
       "y = [{0, 2}, {0, 2, 3, 4, 5}]"]
      ["SETALLEQUAL ([PropAllEqual(x, y)])"]],
     :solutions #{{:x #{0 2 5}, :y #{0 2 5}}}}
    ))
  )
