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
             [not-all-equal [:z 2 :a :b]]]
    :compiled [["z = {1..5}" "a = {1..5}" "b = {1..5}"]
               ["ATLEASTNVALUES ([PropAtLeastNValues(z, cste -- 2, a, ..., cste -- 2)])"]]
    :solutions #{{:z 4, :a 5, :b 2} {:z 5, :a 5, :b 4} {:z 1, :a 4, :b 3}
                 {:z 3, :a 5, :b 3} {:z 5, :a 2, :b 3} {:z 1, :a 3, :b 5}
                 {:z 5, :a 1, :b 2} {:z 1, :a 2, :b 4} {:z 1, :a 1, :b 2}
                 {:z 4, :a 1, :b 3} {:z 4, :a 2, :b 4} {:z 3, :a 5, :b 5}
                 {:z 1, :a 3, :b 3} {:z 5, :a 2, :b 4} {:z 1, :a 1, :b 4}
                 {:z 5, :a 3, :b 2} {:z 5, :a 5, :b 3} {:z 4, :a 5, :b 3}
                 {:z 2, :a 2, :b 1} {:z 2, :a 3, :b 2} {:z 3, :a 2, :b 3}
                 {:z 1, :a 3, :b 1} {:z 3, :a 1, :b 4} {:z 5, :a 5, :b 2}
                 {:z 3, :a 5, :b 2} {:z 2, :a 4, :b 2} {:z 1, :a 3, :b 2}
                 {:z 2, :a 3, :b 5} {:z 4, :a 4, :b 1} {:z 3, :a 3, :b 2}
                 {:z 3, :a 1, :b 2} {:z 1, :a 5, :b 2} {:z 3, :a 3, :b 1}
                 {:z 5, :a 3, :b 4} {:z 1, :a 5, :b 5} {:z 5, :a 4, :b 5}
                 {:z 2, :a 1, :b 2} {:z 2, :a 3, :b 3} {:z 2, :a 5, :b 5}
                 {:z 1, :a 2, :b 5} {:z 4, :a 2, :b 2} {:z 4, :a 3, :b 3}
                 {:z 4, :a 4, :b 3} {:z 2, :a 4, :b 3} {:z 2, :a 2, :b 4}
                 {:z 3, :a 3, :b 3} {:z 5, :a 1, :b 1} {:z 4, :a 4, :b 2}
                 {:z 5, :a 5, :b 1} {:z 3, :a 3, :b 4} {:z 3, :a 1, :b 1}
                 {:z 4, :a 2, :b 5} {:z 5, :a 3, :b 3} {:z 2, :a 4, :b 1}
                 {:z 4, :a 5, :b 5} {:z 4, :a 1, :b 2} {:z 4, :a 5, :b 4}
                 {:z 2, :a 1, :b 1} {:z 2, :a 5, :b 4} {:z 1, :a 2, :b 3}
                 {:z 4, :a 3, :b 5} {:z 1, :a 4, :b 1} {:z 3, :a 2, :b 1}
                 {:z 4, :a 3, :b 4} {:z 3, :a 3, :b 5} {:z 5, :a 1, :b 4}
                 {:z 2, :a 2, :b 5} {:z 2, :a 3, :b 4} {:z 1, :a 5, :b 4}
                 {:z 4, :a 2, :b 3} {:z 3, :a 2, :b 2} {:z 3, :a 4, :b 4}
                 {:z 5, :a 4, :b 4} {:z 5, :a 4, :b 3} {:z 4, :a 1, :b 4}
                 {:z 3, :a 5, :b 1} {:z 5, :a 2, :b 5} {:z 5, :a 2, :b 1}
                 {:z 2, :a 5, :b 3} {:z 2, :a 4, :b 4} {:z 3, :a 1, :b 5}
                 {:z 2, :a 1, :b 4} {:z 3, :a 2, :b 4} {:z 1, :a 2, :b 2}
                 {:z 2, :a 5, :b 1} {:z 5, :a 3, :b 5} {:z 3, :a 2, :b 5}
                 {:z 1, :a 4, :b 2} {:z 2, :a 1, :b 3} {:z 1, :a 1, :b 5}
                 {:z 4, :a 3, :b 1} {:z 1, :a 1, :b 1} {:z 2, :a 1, :b 5}
                 {:z 1, :a 1, :b 3} {:z 5, :a 4, :b 2} {:z 3, :a 4, :b 3}
                 {:z 1, :a 3, :b 4} {:z 1, :a 4, :b 4} {:z 1, :a 2, :b 1}
                 {:z 4, :a 2, :b 1} {:z 3, :a 4, :b 2} {:z 3, :a 4, :b 5}
                 {:z 2, :a 5, :b 2} {:z 5, :a 2, :b 2} {:z 3, :a 5, :b 4}
                 {:z 4, :a 4, :b 5} {:z 3, :a 4, :b 1} {:z 5, :a 4, :b 1}
                 {:z 2, :a 2, :b 3} {:z 4, :a 1, :b 1} {:z 5, :a 1, :b 5}
                 {:z 4, :a 3, :b 2} {:z 5, :a 3, :b 1} {:z 4, :a 1, :b 5}
                 {:z 5, :a 1, :b 3} {:z 4, :a 4, :b 4} {:z 5, :a 5, :b 5}
                 {:z 4, :a 5, :b 1} {:z 1, :a 5, :b 3} {:z 2, :a 3, :b 1}
                 {:z 1, :a 5, :b 1} {:z 2, :a 4, :b 5} {:z 3, :a 1, :b 3}
                 {:z 1, :a 4, :b 5}}
    })
  )
