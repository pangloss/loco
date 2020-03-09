(ns loco.constraints.not-all-equal-test
  (:use
   loco.constraints
   clojure.test)
  (:require
   [loco.model :as model]
   [loco.compiler :as compiler]
   [loco.solver :as solver]
   [loco.constraints.test-utils :refer :all])
 )

(deftest not-all-equal-test
  (test-loco
   [($in :z 1 5)
    ($in :a 1 5)
    ($in :b 1 5)
    ($!= :z 2 :a :b)]
   {:model '[[:var :z :public [:int 1 5]]
             [:var :a :public [:int 1 5]]
             [:var :b :public [:int 1 5]]
             [:var :2 :hidden [:const 2]]
             [not-all-equal [:z :2 :a :b]]]
    :compiled [[""]]
    :solutions #{{:x 2, :y 1, :z 1}
                 {:x 3, :y 1, :z 1}
                 {:x 4, :y 1, :z 1}
                 {:x 5, :y 1, :z 1}
                 {:x 1, :y 1, :z 2}
                 {:x 2, :y 1, :z 2}
                 {:x 3, :y 1, :z 2}
                 {:x 4, :y 1, :z 2}
                 {:x 5, :y 1, :z 2}}
    })
  )
