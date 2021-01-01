(ns loco.constraints.count-test
  (:require
   [clojure.test :refer :all]
   [loco.constraints :refer :all]
   [loco.constraints.test-utils :refer :all]
   ))

(deftest count-test
  (test-loco 1
    [($in :limit 0 5)
     ($count 3 [1 2 3] :limit)]
    {:solutions #{{:limit 1}}}
    )

  (test-loco 2
    [($in :value 0 5)
     ($count :value [1 2 3 3] 2)]
    {:solutions #{{:value 3}}}
    )

  (test-loco 3
    [($in :limit 0 5)
     ($in :a 0 5)
     ($count 3 [1 2 3 :a] :limit)]
    {:solutions
     #{{:limit 1, :a 5} {:limit 1, :a 1} {:limit 1, :a 4}
       {:limit 1, :a 2} {:limit 1, :a 0} {:limit 2, :a 3}}}
    )

  (test-loco 4
    [($in :limit 0 5)
     ($in :value 1 3)
     ($in :a 0 5)
     ($count :value [1 2 3 :a] :limit)]
    {:solutions
     #{
       {:limit 1, :value 1, :a 0}
       {:limit 1, :value 1, :a 2}
       {:limit 1, :value 1, :a 3}
       {:limit 1, :value 1, :a 4}
       {:limit 1, :value 1, :a 5}
       {:limit 1, :value 2, :a 0}
       {:limit 1, :value 2, :a 1}
       {:limit 1, :value 2, :a 3}
       {:limit 1, :value 2, :a 4}
       {:limit 1, :value 2, :a 5}
       {:limit 1, :value 3, :a 0}
       {:limit 1, :value 3, :a 1}
       {:limit 1, :value 3, :a 2}
       {:limit 1, :value 3, :a 4}
       {:limit 1, :value 3, :a 5}
       {:limit 2, :value 1, :a 1}
       {:limit 2, :value 2, :a 2}
       {:limit 2, :value 3, :a 3}
       }}
    )
  )
