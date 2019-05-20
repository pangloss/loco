(ns ^:solutions loco.core-test
  (:use clojure.test
        loco.constraints)
  (:require
   [loco.solver :as solver]
   [loco.model :as model]
))

(defmacro test-constraint-model
  ([docstring model solution-maps]
   `(is
     (=
      (set ~solution-maps)
      (set (solver/solutions ~model)))
     ~docstring))
  ([model solution-maps]
   `(test-constraint-model nil ~model ~solution-maps)))

(deftest basic-tests
  (test-constraint-model
   "Basic test with only 1 var"
   [($in :y 1 3)]
   [{:y 1} {:y 3} {:y 2}])

  (test-constraint-model
   "Basic test with only 1 var, bounded"
   [($in :y 1 3 :bounded)]
   [{:y 1} {:y 2} {:y 3}])


  (test-constraint-model
   "Basic test with only 1 var, bounded, using $="
   [
    ($in :y 1 3 :bounded)
    ($= :y 2)
    ]
   [{:y 2}])

  (test-constraint-model
   "Basic test with only 1 var, bounded, using $="
   [
    ($in :y  1 3 :bounded)
    ($in :_x 2 2 :bounded)
    ($= :y :_x)
    ]
   [{:y 2}]))

(deftest basic-test2
  (test-constraint-model
   "Basic test case with $= and bounded vars "
   [($in :x 1 3)
    ($in :y 1 3 :bounded)
    ($in :z [1 2 3])
    ($= :x :y)
    ($= :y :z)]
   [{:x 1 :y 1 :z 1} {:x 2 :y 2 :z 2} {:x 3 :y 3 :z 3}]))

(deftest arithmetic-test
  (test-constraint-model
   [($in :x 0 5)
    ($= ($+ :x) 5)]
   [{:x 5}])

  (test-constraint-model
   [($in :x 0 5)
    ($= ($+ :x :x) 10)]
   [{:x 5}])

  (test-constraint-model
   [($in :x 0 5)
    ($= ($- :x 5) 0)]
   [{:x 5}])

  (test-constraint-model
   [($in :x 0 5)
    ($in :y 0 5)
    ($= ($- :x :y) :y)]
   #{{:x 0, :y 0} {:x 2, :y 1} {:x 4, :y 2}})

  (test-constraint-model
   [($in :x 0 5)
    ($in :y 0 5)
    ($= ($- :x :y) ($- :y :x))]
   #{{:x 0, :y 0}
     {:x 1, :y 1}
     {:x 2, :y 2}
     {:x 4, :y 4}
     {:x 3, :y 3}
     {:x 5, :y 5}})

  (test-constraint-model
   [($in :x 0 5)
    ($= ($* :x :x) 0)]
   [{:x 0}]))

(deftest arithmetic-test2
  (test-constraint-model
   [($in :x -5 5)
    ($in :y -5 5)
    ($in :z -5 5)
    ($= ($+ :x :y) 5)
    ($= ($- :x :z) 2)
    ($= ($* :y :z) 2)]
   [{:z 1, :y 2, :x 3} {:z 2, :y 1, :x 4}]))

(deftest abs-test
  (test-constraint-model
   [($in :x -5 5)
    ($= ($abs :x) 2)]
   [{:x -2} {:x 2}]))

(deftest minmax-test
  (test-constraint-model
   [($in :x -5 5)
    ($in :y -5 5)
    ($in :z -5 5)
    ($= ($min :x :y :z) :x)
    ($= ($max :x :y :z) :z)
    ($= :x -5)
    ($= :z -5)]
   [{:x -5 :y -5 :z -5}])
  (test-constraint-model
   [($in :x 1 5)
    ($in :y 2 6)
    ($in :z 3 7)
    ($= ($min :x 5) 5)
    ($= ($max :z 3) 3)
    ($= :x :y)]
   [{:x 5 :z 3 :y 5}]))



(deftest mod-scalar-test
  (test-constraint-model
   [($in :x 1 5)
    ($in :y 1 5)
    ($in :z 1 5)
    ($= ($mod :x :y) 4)
    ($= ($scalar [:x :y :z] '(1 1 -2)) 3)]
   [{:x 4 :y 5 :z 3}]))

(deftest eq-ineq-test
  (test-constraint-model
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
   [{:x 1 :y 2 :z 2}]))

(deftest logic-test
  (test-constraint-model
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
    [{:x 1}]))

(deftest reify-test
  (test-constraint-model
   [($in :x 0 1)
    ($reify :true ($true))
    ($reify :false ($false))
    ($= :true :x)
    ($= :false ($- 1 :x))]
   [{:x 1}]))

(deftest circuit-test
  (-> (solver/solution
        [($in :a 0 4)
         ($in :b [0])
         ($in :c 0 4)
         ($in :d 0 4)
         ($in :e 0 4)
         ($circuit [:a :b :c :d :e])])
    (as-> sol
          (let [a [:a :b :c :d :e]
                [v i] (first sol)
                w (a i)
                i (sol w)
                x (a i)
                i (sol x)
                y (a i)
                i (sol y)
                z (a i)]
            (is (= (count (distinct [v w x y z])) 5)))))

  ;;testing offset
  (-> (solver/solution
        [($in :a 1 5)
         ($in :b [1])
         ($in :c 1 5)
         ($in :d 1 5)
         ($in :e 1 5)
         ($circuit [:a :b :c :d :e] 1)])
    (as-> sol
          (let [a [:a :b :c :d :e]
                [v i] (first sol)
                w (a (dec i))
                i (sol w)
                x (a (dec i))
                i (sol x)
                y (a (dec i))
                i (sol y)
                z (a (dec i))]
            (is (= (count (distinct [v w x y z])) 5))))))



(deftest cardinality-test
  (-> (solver/solutions
        [($in :a 1 5)
         ($in :b 1 5)
         ($in :c 1 5)
         ($in :d 1 5)
         ($in :e 1 5)
         ($in :ones 1 5)
         ($in :twos 1 5)
         ($cardinality [:a :b :c :d :e] {1 :ones 2 :twos})])
    (as-> ms
          (doseq [m ms]
            (-> m
              (map [:a :b :c :d :e])
              frequencies
              (map [1 2])
              (= (map m [:ones :twos]))
              is)))))

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

(deftest tricky-test-0-2-1
  "Target specific bug fixed by 0.2.1"
  (test-constraint-model
   [($in :a [5])
    ($in :b 0 1)
    ($reify :reify ($< 0 :a))
    ($= :b :reify)]
   [{:a 5 :b 1}]))

(deftest knapsack-test
  (test-constraint-model
   "Super basic knapsack constraint"
   [($knapsack [1 1] [1 1] [1 1] 2 2)]
   [{}])

  (test-constraint-model
   "Basic knapsack constraint"
   [($knapsack [2 3]
               [6 5]
               [:x :y]
               :total-weight
               :total-value)
    ($in :x 0 10)
    ($in :y 0 10)
    ($in :total-weight 0 10)
    ($in :total-value 0 10)]
   [{:x 0 :y 0 :total-weight 0 :total-value 0}
    {:x 1 :y 0 :total-weight 2 :total-value 6}
    {:x 0 :y 1 :total-weight 3 :total-value 5}
    {:x 0 :y 2 :total-weight 6 :total-value 10}])

  (test-constraint-model
   "Knapsack constraint with negatives"
   [($in :x -100 100)
    ($in :y -100 100)
    ($knapsack [1 1] [10 20] [:x :y] 10 10)]
   [{:x 19 :y -9}]))
