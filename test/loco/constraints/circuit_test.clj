(ns loco.constraints.circuit-test
  (:require
   [clojure.test :refer :all]
   [loco.constraints :refer :all]
   [loco.constraints.test-utils :refer :all]
   ))

(deftest circuit-test
  (is
   (loco?
    [($in :a 0 5)
     ($in :b 0 5)
     ($in :c 0 5)
     ($circuit [:a :b :c])]
    {:solutions
     #{
       {:a 1, :b 2, :c 0} ;; -> [0 -> 1] [1 -> 2] [2 -> 0]
       {:a 2, :b 0, :c 1} ;; -> [0 -> 2] [2 -> 1] [1 -> 0]
       }}))

  (is
   (loco?
    [($in :a 0 5)
     ($in :b 0 5)
     ($in :c 0 5)
     ($circuit [:a :b :c] 1)]
    {:solutions
     #{{:a 2, :b 3, :c 1}
       {:a 3, :b 1, :c 2}}}))

  (is
   (loco?
    [($in :a 0 5)
     ($in :b 0 5)
     ($in :c 0 5)
     ($in :d 0 5)
     ($circuit [:a :b :c :d])]
    {:solutions
     #{
       {:a 1, :b 2, :c 3, :d 0}
       {:a 1, :b 3, :c 0, :d 2}
       {:a 2, :b 0, :c 3, :d 1}
       {:a 2, :b 3, :c 1, :d 0}
       {:a 3, :b 0, :c 1, :d 2}
       {:a 3, :b 2, :c 0, :d 1}
       }}))

  )
