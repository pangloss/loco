(ns loco.constraints.int-value-precede-chain-test
  (:require
   [clojure.test :refer :all]
   [loco.constraints :refer :all]
   [loco.constraints.test-utils :as utils]
   ))

(deftest int-value-precede-chain-test
  (testing "GCCAT example: http://sofdem.github.io/gccat/gccat/Cint_value_precede_chain.html"
    (is
     (loco?
      [($in :a 4)
       ($int-value-precede-chain [:a ,0,6,1,0] [4,0,1])]
      {
       :solutions #{{:a 4}}}
      ))

    (is
     (loco?
      [($in :a 0 10)
       ($int-value-precede-chain [:a ,0,6,1,0] [4,0,1])]
      {
       :solutions #{{:a 4}}}
      ))
    
    (is
     (loco?
      [($in :a [0 4])
       ($in :b 0)
       ($in :c 6)
       ($in :d [1 3])
       ($in :e [1 2])
       ($int-value-precede-chain [:a :b :c :d :e] [4,0,1])]
      {:solutions
       #{
         {:a 4, :b 0, :c 6, :d 1, :e 1}
         {:a 4, :b 0, :c 6, :d 3, :e 1}
         {:a 4, :b 0, :c 6, :d 1, :e 2}
         {:a 4, :b 0, :c 6, :d 3, :e 2} ;; this is curious, we didn't see a 1 after 0, but i guess this is OK?
         }}
      ))

    (is
     (loco?
      [($in :e [0 5])
       ($int-value-precede-chain [4 0 :e] [4,0,1])]
      {:solutions
       #{
         {:e 5}
         {:e 0} ;;why is it that this is OK?
         }};; also, why where is {:e 1}?
      ))

    (is
     (loco?
      [($in :e [0 5])
       ($int-value-precede-chain [4 0 :e 2] [4,0,1 2])]
      {:solutions #{}}
      ))
    )

  (is
     (loco?
      [($in :a 0 1)
       ($in :b 0 2)
       ($in :c 0 3)
       ($in :d 0 4)
       ($int-value-precede-chain [:a :b :c :d] [0 1 2])]
      {:solutions
       #{
	 {:a 0, :b 0, :c 0, :d 0}
	 {:a 0, :b 0, :c 0, :d 3}
	 {:a 0, :b 0, :c 0, :d 4}
	 {:a 0, :b 0, :c 1, :d 1}
	 {:a 0, :b 0, :c 1, :d 3}
	 {:a 0, :b 0, :c 3, :d 1}
	 {:a 0, :b 1, :c 0, :d 1}
	 {:a 0, :b 1, :c 0, :d 2}
	 {:a 0, :b 1, :c 0, :d 3}
	 {:a 0, :b 1, :c 1, :d 2}
	 {:a 0, :b 1, :c 2, :d 0}
	 {:a 0, :b 1, :c 2, :d 2}
	 {:a 0, :b 1, :c 2, :d 4}
	 {:a 0, :b 1, :c 3, :d 0}
	 {:a 0, :b 1, :c 3, :d 1}
	 {:a 0, :b 1, :c 3, :d 2}
         {:a 0, :b 0, :c 0, :d 1}
         {:a 0, :b 0, :c 1, :d 0}
         {:a 0, :b 0, :c 1, :d 2}
         {:a 0, :b 0, :c 1, :d 4}
         {:a 0, :b 0, :c 3, :d 0}
         {:a 0, :b 0, :c 3, :d 3}
         {:a 0, :b 0, :c 3, :d 4}
         {:a 0, :b 1, :c 0, :d 0}
         {:a 0, :b 1, :c 0, :d 4}
         {:a 0, :b 1, :c 1, :d 0}
         {:a 0, :b 1, :c 1, :d 1}
         {:a 0, :b 1, :c 1, :d 3}
         {:a 0, :b 1, :c 1, :d 4}
         {:a 0, :b 1, :c 2, :d 1}
         {:a 0, :b 1, :c 2, :d 3}
         {:a 0, :b 1, :c 3, :d 3}
         {:a 0, :b 1, :c 3, :d 4}
         }}
      ))
  )
