(ns loco.constraints.views.minus-test
  (:require
   [clojure.test :refer :all]
   [loco.constraints :refer :all]
   [loco.constraints.test-utils :refer :all]
   ))

(deftest minus-test
  (test-loco
   [($in :a -5 5)
    ($= :a ($minus-view 3))
    ]
   {:identity '[[:var :a :public [:int -5 5]]
                [arithm [:a = [minus 3 []]]]],
    :model '[[:var :a :public [:int -5 5]]
             [:view "-3" [minus 3 []] [:int -3 -3]]
             [arithm [:a = "-3"]]],
    :compiled [["a = {-5..5}" "cste -- -3 = -3"] ["ARITHM ([a = -3])"]],
    :solutions #{{:a -3}}})
  )
