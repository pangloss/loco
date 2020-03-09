(ns loco.constraints.circuit-test
  (:require
   [clojure.test :refer :all]
   [loco.constraints :refer :all]
   [loco.constraints.test-utils :refer :all]
   ))

(deftest circuit-test
  (test-loco
   [($in :a 0 5)
    ($in :b 0 5)
    ($in :c 0 5)
    ($circuit [:a :b :c])]
   {:solutions
    #{
      {:a 1, :b 2, :c 0} ;; -> [0 -> 1] [1 -> 2] [2 -> 0]
      {:a 2, :b 0, :c 1} ;; -> [0 -> 2] [2 -> 1] [1 -> 0]
      }})

  (test-loco
   [($in :a 0 5)
    ($in :b 0 5)
    ($in :c 0 5)
    ($circuit [:a :b :c] 1)]
   {:solutions
    #{{:a 2, :b 3, :c 1}
      {:a 3, :b 1, :c 2}}})

  (test-loco
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
      }})

  )

;; legacy test
#_(deftest circuit-test
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
