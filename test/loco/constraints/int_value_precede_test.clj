(ns loco.constraints.int-value-precede-test
  (:require
   [clojure.test :refer :all]
   [loco.constraints :refer :all]
   [loco.constraints.test-utils :refer :all]
   ))

(deftest int-value-precede-test
  (testing "GCCAT example: http://sofdem.github.io/gccat/gccat/Cint_value_precede.html"
    (test-loco
     [($in :a 4)
      ($int-value-precede [:a 0 6 1 0] 0 1)]
     {:solutions
      #{{:a 4}}}
     )
    )

  (test-loco
   [($in :a 0 4)
    ($in :b 0 3)
    ($int-value-precede [:a :b] 0 1)]
   {:solutions
    #{
      {:a 0, :b 0}
      {:a 0, :b 1}
      {:a 0, :b 2}
      {:a 0, :b 3}
      {:a 2, :b 0}
      {:a 2, :b 2}
      {:a 2, :b 3}
      {:a 3, :b 0}
      {:a 3, :b 2}
      {:a 3, :b 3}
      {:a 4, :b 0}
      {:a 4, :b 2}
      {:a 4, :b 3}
      }}
   )
  

  (test-loco
   [($in :c 0 5)
    ($int-value-precede [2 3 :c] 0 1)]
   {:solutions
    #{
      {:c 0}
      {:c 2}
      {:c 3}
      {:c 4}
      {:c 5}
      }}
   )
  
  
  )
