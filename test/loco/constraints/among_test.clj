(ns loco.constraints.among-test
  (:require
   [clojure.test :refer :all]
   [loco.constraints :refer :all]
   [loco.constraints.test-utils :refer :all]
   )
  )

#_(testing "among"
  (constraints-assert
   '("AMONG ([AMONG([a = {0..5},b = {0..5},c = {0..5}],{[2, 3]},3 = 3)])")
   [($in :a 0 5)
    ($in :b 0 5)
    ($in :c 0 5)
    ($in :limit 0 3)
    ($among 3 [:a :b :c] [2 3])]))

(deftest among-test
  (test-loco
   [($in :a 0 5)
    ($in :b 0 5)
    ($in :c 0 5)
    ;;($in :limit 0 3)
    ($among 3 [:a :b :c] [2])]
   {:model
    '[[:var :a :public [:int 0 5]]
      [:var :b :public [:int 0 5]]
      [:var :c :public [:int 0 5]]
      [among [[:a :b :c] [nb-var 3] [values [2]]]]],
    :compiled
    [["a = {0..5}" "b = {0..5}" "c = {0..5}"]
     ["AMONG ([AMONG([a = {0..5},b = {0..5},c = {0..5}],{[2]},cste -- 3 = 3)])"]],
    :solutions #{{:a 2, :b 2, :c 2}}}
   )

  (test-loco
   [($in :a 0 2)
    ($in :b 0 2)
    ($in :c 0 2)
    ($in :_limit 2 3)
    ($among :_limit [:a :b :c] [2])]
   {:model
    '[[:var :a :public [:int 0 2]]
      [:var :b :public [:int 0 2]]
      [:var :c :public [:int 0 2]]
      [:var :_limit :hidden [:int 2 3]]
      [among [[:a :b :c] [nb-var :_limit] [values [2]]]]],
    :compiled
    [["a = {0..2}" "b = {0..2}" "c = {0..2}" "_limit = {2..3}"]
     ["AMONG ([AMONG([a = {0..2},b = {0..2},c = {0..2}],{[2]},_limit = {2..3})])"]],
    :solutions
    #{{:a 2, :b 2, :c 2} {:a 2, :b 2, :c 1} {:a 2, :b 0, :c 2}
      {:a 0, :b 2, :c 2} {:a 2, :b 2, :c 0} {:a 1, :b 2, :c 2}
      {:a 2, :b 1, :c 2}}}
   )

  )
