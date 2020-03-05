(ns loco.core-test
  (:require
   [clojure.test :refer :all]
   [loco.constraints :refer :all]
   [loco.constraints.test-utils :refer :all]
   [loco.solver :as solver]
   ))

#_(deftest logic-test
  (is (loco?
       [($in :x [1])
        ($true)
        ($not ($false))
        ($not ($not ($true)))
        ($and ($true) ($true))
        ($not ($and ($true) ($false)))
        ($or ($true) ($false))
        ($if ($true) ($true) ($false))
        ($when ($false) ($false))
        ($if ($false) ($false) ($true))
        ($cond
         ($false) ($true)
         ($false) ($false)
         ($true) ($true)
         ($false) ($true)
         :else ($true))]
       {
        :model []
        ;;:solutions #{{:x 1}}
        })))

(deftest arithmetic-test
  (is
   (loco?
    [($in :x 0 5)
     ($= ($+ :x) 5)]
    {:solutions #{{:x 5}}}))

  (is (loco?
       [($in :x 0 5)
        ($= ($+ :x :x) 10)]
       {:solutions #{{:x 5}}}))

  (is (loco? [($in :x 0 5)
              ($= ($- :x 5) 0)]
             {:solutions #{{:x 5}}}))

  (is (loco?
       [($in :x 0 5)
        ($in :y 0 5)
        ($= ($- :x :y) :y)]
       {:solutions #{{:x 0, :y 0}
                     {:x 2, :y 1}
                     {:x 4, :y 2}}}))

  (is (loco?
       [($in :x 0 5)
        ($in :y 0 5)
        ($= ($- :x :y) ($- :y :x))]
       {:solutions #{{:x 0, :y 0}
                     {:x 1, :y 1}
                     {:x 2, :y 2}
                     {:x 4, :y 4}
                     {:x 3, :y 3}
                     {:x 5, :y 5}}}))

  (is (loco? [($in :x 0 5)
              ($= ($* :x :x) 0)]
             {:solutions #{{:x 0}}})))

(deftest arithmetic-test2
  (is (loco?
       [($in :x -5 5)
        ($in :y -5 5)
        ($in :z -5 5)
        ($= ($+ :x :y) 5)
        ($= ($- :x :z) 2)
        ($= ($* :y :z) 2)]
       {:solutions #{{:z 1, :y 2, :x 3}
                     {:z 2, :y 1, :x 4}}})))

(deftest mod-scalar-test
  (is (loco?
       [($in :x 1 5)
        ($in :y 1 5)
        ($in :z 1 5)
        ($= ($mod :x :y) 4)
        ($= ($scalar [:x :y :z] '(1 1 -2)) 3)]
       {:solutions #{{:x 4 :y 5 :z 3}}})))

(deftest eq-ineq-test
  (is (loco?
       [($in :x 1 5)
        ($in :y 1 5)
        ($in :z 1 5)
        ($= :z 2)
        ($< :x :y)
        ($<= :y :z)
        ($> :y :x)
        ($>= :z :y)
        ($!= :x :y)
        ($!= :x :y :z)]
       {:solutions #{{:x 1 :y 2 :z 2}}})))

;; FIXME: $reify
#_(deftest reify-test
  (is (loco?
       [($in :x 0 1)
        ($reify :true ($true))
        ($reify :false ($false))
        ($= :true :x)
        ($= :false ($- 1 :x))]
       {:solutions #{{:x 1}}})))


(deftest optimization-test
  (is (= (solver/solution [($in :x 1 5)]
                          :maximize :x)
         {:x 5}))

  (is (= (solver/solution [($in :x 1 5)]
                          :minimize :x)
         {:x 1}))

  (is (= (solver/solution [($in :x 1 5) ($< :x 0)]
                          :maximize :x)
         nil))

  (is (= (solver/solution [($in :x 1 5) ($< :x 0)]
                          :minimize :x)
         nil))
  )

#_(deftest tricky-test-0-2-1
  "Target specific bug fixed by 0.2.1"
  (is (loco?
       [($in :a [5])
        ($in :b 0 1)
        ($reify :reify ($< 0 :a))
        ($= :b :reify)]
       {:solutions #{{:a 5 :b 1}}})))
