(ns loco.constraints.vars-test
  (:require [loco.compiler :as compiler]
            [loco.model :as model]
            [loco.solver :as solver])
  (:use clojure.test loco.model.test)
  (:require
   [loco.constraints :refer :all]
   [loco.constraints.test-utils :refer :all]
   )
  (:import org.chocosolver.solver.Model))

(deftest const-vars-test
  (testing "$const"
    (are [in out] (= out in)
      ($const :7 7)               [:var :7 :public [:const 7]]
      ($const :a 1)               [:var :a :public [:const 1]]
      ($const [:b 10] 2)          [:var [:b 10] :public [:const 2]]
      ($const [:constraint 10] 2) [:var [:constraint 10] :public [:const 2]]
      ($const- [:constraint 20] 4) [:var [:constraint 20] :hidden [:const 4]]
      ($const [:_constraint 30] 4) [:var [:_constraint 30] :hidden [:const 4]]
      )
    )

  (testing "model/compile"
    (are [in out] (= out (model/compile in))
      [($const :7 7)]               [[:var :7 :public [:const 7]]]
      [($const :a 1)]               [[:var :a :public [:const 1]]]
      [($const [:b 10] 2)]          [[:var "[:b 10]" :public [:const 2]]]
      [($const [:constraint 10] 2)] [[:var "[:constraint 10]" :public [:const 2]]]
      )
    )

  (testing "compiler/compile"
    (choco-vars-string-assert
     '("7 = 7"
       "a = 7"
       "b = 4"
       "[:constraint 10] = 2")
     [($const :7 7)
      ($const :a 7)
      ($const- :b 4)
      ($const [:constraint 10] 2)])
    )

  (testing "solutions"
    (are [input expected] (= expected (solver/solutions input))
      [($const :a 0)]            '({:a 0})
      [($const- :a 0)]           '({})
      [($const- :aa 0)
       ($const :aaa 0)]          '({:aaa 0})
      )
    )
  )

(deftest bool-vars-test
  (testing "$bool"
    (are [in out] (= out in)
      ($bool :8)               [:var :8 :public [:bool 0 1]]
      ($bool [:constraint 11]) [:var [:constraint 11] :public [:bool 0 1]]
      ($bools :a :b)           [[:var :a :public [:bool 0 1]]
                                [:var :b :public [:bool 0 1]]]
      ($bools [:a 1] [:_b 2])   [[:var [:a 1] :public [:bool 0 1]]
                                [:var [:_b 2] :hidden [:bool 0 1]]]
      )
    )

  (testing "model/compile"
    (are [in out] (= out (model/compile in))
      [($bool :8)]               [[:var :8 :public [:bool 0 1]]]
      [($bool [:constraint 11])] [[:var "[:constraint 11]" :public [:bool 0 1]]]
      [($bools :a :b)]           [[:var :a :public [:bool 0 1]]
                                  [:var :b :public [:bool 0 1]]]
      ($bools [:a 1] [:b 2])   [[:var "[:a 1]" :public [:bool 0 1]]
                                [:var "[:b 2]" :public [:bool 0 1]]]
      )
    )

  (testing "compiler/compile"
    (choco-vars-string-assert
     '("8 = [0,1]"
       "[:constraint 11] = [0,1]"
       "[:a 1] = [0,1]"
       "[:b 2] = [0,1]")
     [($bool :8)
      ($bool [:constraint 11])
      ($bools [:a 1] [:b 2])]
     )
    )

  (testing "solutions"
    (are [input expected] (= expected (solver/solutions input))
      [($bool :a)]     '({:a 0} {:a 1})
      [($bool [:a 1])] '({[:a 1] 0} {[:a 1] 1})
      [($bool- :a)]    '({} {})
      [($bools :a :b)] '({:a 0, :b 0} {:a 1, :b 0} {:a 0, :b 1} {:a 1, :b 1})
      )
    )
  )

(deftest int-vars-test
  (testing "$in"
    (are [in out] (= out in)
      ($in :a 1)                 [:var :a :public [:int 1 1]]
      ($in :b 2 2)               [:var :b :public [:int 2 2]]
      ($in :c 3 4)               [:var :c :public [:int 3 4]]
      ($in [:constraint 12] 5)   [:var [:constraint 12] :public [:int 5 5]]
      ($in :d [6 7 8 9])         [:var :d :public [:int [6 7 8 9]]]
      ($in :f 10 20 :bounded)    [:var :f :public [:int 10 20 :bounded]]
      ($in :_g 10 20 :bounded)   [:var :_g :hidden [:int 10 20 :bounded]]
      ($in :_d [6 7 8 9])         [:var :_d :hidden [:int [6 7 8 9]]]
      ($in [:_constraint 12] 5)   [:var [:_constraint 12] :hidden [:int 5 5]]
      )
    )

  (testing "model/compile"
    (are [in out] (= out (model/compile in))
      [($in :a 1)]                 [[:var :a :public [:int 1 1]]]
      [($in :b 2 2)]               [[:var :b :public [:int 2 2]]]
      [($in :c 3 4)]               [[:var :c :public [:int 3 4]]]
      [($in [:constraint 12] 5)]   [[:var "[:constraint 12]" :public [:int 5 5]]]
      [($in :d [6 7 8 9])]         [[:var :d :public [:int [6 7 8 9]]]]
      [($in :f 10 20 :bounded)]    [[:var :f :public [:int 10 20 :bounded]]]
      )
    )

  (testing "compiler/compile"
    (choco-vars-string-assert
     '("a = 1"
       "b = 2"
       "c = {3..4}"
       "[:constraint 12] = 5"
       "d = {6..9}"
       "f = [10,20]")
     [($in :a 1)
      ($in :b 2 2)
      ($in :c 3 4)
      ($in [:constraint 12] 5)
      ($in :d [6 7 8 9])
      ($in :f 10 20 :bounded)]
     )
    )

  (testing "solutions"
    (are [input expected] (= expected (solver/solutions input))
      [($int :a 0)]              '({:a 0})
      [($int :a 0 4)]            '({:a 0} {:a 1} {:a 2} {:a 3} {:a 4})
      [($int :a [3 5 8 13])]     '({:a 3} {:a 5} {:a 8} {:a 13})
      [($int [:a 1] 0 2)]        '({[:a 1] 0} {[:a 1] 1} {[:a 1] 2})
      )
    )
  )

(deftest set-vars-test
  (testing "$set"
    (are [in out] (= out in)
      ($set :a [] [])       [:var :a :public [:set #{} #{}]]
      ($set :b [] [1 2 3])  [:var :b :public [:set #{} #{1 2 3}]]
      ($set :d [1] [1 2 3]) [:var :d :public [:set #{1} #{1 2 3}]]
      ($set :e [1 2 3])     [:var :e :public [:set #{1 2 3}]]
      ($set [:constraint 2] [1 2 3]) [:var [:constraint 2] :public [:set #{1 2 3}]]
      ($set [:_constraint 2] [1 2 3]) [:var [:_constraint 2] :hidden [:set #{1 2 3}]]
      ($set :_e [1 2 3])     [:var :_e :hidden [:set #{1 2 3}]]
      )
    )

  (testing "model/compile"
    (are [in out] (= out (model/compile in))
      [($set :a [] [])]       [[:var :a :public [:set #{} #{}]]]
      [($set :b [] [1 2 3])]  [[:var :b :public [:set #{} #{1 2 3}]]]
      [($set :d [1] [1 2 3])] [[:var :d :public [:set #{1} #{1 2 3}]]]
      [($set :e [1 2 3])]     [[:var :e :public [:set #{1 2 3}]]]

      [($set [:constraint 2] [1 2 3])]
      [[:var "[:constraint 2]" :public [:set #{1 2 3}]]]
      )
    )

  (testing "compiler/compile"
    (choco-vars-string-assert
     '("a = {}"
       "b = [{}, {1, 2, 3}]"
       "d = [{1}, {1, 2, 3}]"
       "e = {1, 2, 3}"
       "[:constraint 2] = {1, 2, 3}")
     [($set :a [] [])
      ($set :b [] [1 2 3])
      ($set :d [1] [1 2 3])
      ($set :e [1 2 3])
      ($set [:constraint 2] [1 2 3])])
    )

  (testing "solutions"
    (are [input expected] (= expected (solver/solutions input))
      [($set :a [] [])]  '({:a #{}})
      [($set :a [] [0])] '({:a #{0}} {:a #{}})
      [($set :a [0] [0])] '({:a #{0}})
      [($set :a [0] [0 1])] '({:a #{0 1}} {:a #{0}})
      )
    )
  )

(deftest task-vars-test
  (testing "$task"
    (are [in out] (= out in)
      ($task :my-task [1 2] [2 3] [3 4]) [:var :my-task :public [:task [1 2] [2 3] [3 4]]]
      ($task :my-task :a :b :c) [:var :my-task :public [:task :a :b :c]]
      ($task [:my-task 1] :a :b :c) [:var [:my-task 1] :public [:task :a :b :c]]
      ($task :_my-task [1 2] [2 3] [3 4]) [:var :_my-task :hidden [:task [1 2] [2 3] [3 4]]]
      ($task [:_my-task 1] :a :b :c) [:var [:_my-task 1] :hidden [:task :a :b :c]]
      )
    )

  (testing "model/compile"
    (are [in out] (= out (model/compile in))
      [($task :my-task [1 2] [2 3] [3 4])]
      [[:var :my-task :public [:task [1 2] [2 3] [3 4]]]]

      [($task :my-task :a :b :c)]
      [[:var :my-task :public [:task :a :b :c]]]

      [($task [:my-task 1] :a :b :c)]
      [[:var "[:my-task 1]" :public [:task :a :b :c]]]
      )
    )

  (testing "compiler/compile"
    (choco-vars-string-assert
     '("a = 1"
       "b = 2"
       "c = 3"
       "Task[start=IV_1 = {1..2}, duration=IV_2 = {2..3}, end=IV_3 = {3..4}]"
       "Task[start=a = 1, duration=b = 2, end=c = 3]"
       "Task[start=a = 1, duration=b = 2, end=c = 3]")
     [($in :a 1)
      ($in :b 2)
      ($in :c 3)
      ($task :my-task [1 2] [2 3] [3 4])
      ($task :my-task :a :b :c)
      ($task [:my-task 1] :a :b :c)]
     )
    )

  (testing "solutions"
    (are [input expected] (= expected (solver/solutions input))
      [($task :task [0 1] [0 1] [0 1])]
      '({:task {:start 0, :duration 0, :end 0}}
        {:task {:start 0, :duration 1, :end 1}}
        {:task {:start 1, :duration 0, :end 1}})
      [($int- :start [0 1])
       ($int- :duration 0)
       ($int- :end [0 1])
       ($task :task :start :duration :end)]
      '({:task {:start 0, :duration 0, :end 0}}
        {:task {:start 1, :duration 0, :end 1}})
      [($int- :start [0 1])
       ($int- :end [0 1])
       ($task :task :start [1] :end)]
      '({:task {:start 0, :duration 1, :end 1}})
      [($int- :start [0 1])
       ($int- :end [0 3 5])
       ($task :task :start [[1 3 5]] :end)]
      '({:task {:start 0, :duration 3, :end 3}}
        {:task {:start 0, :duration 5, :end 5}})
      )
    )
  )

(deftest tuples-test
  (testing "$tuples"
    (are [in out] (= out in)
      ($tuples :tuple1 [[1 2] [0 3]])
      [:var :tuple1 :hidden [:tuples :allowed [[1 2] [0 3]]]]

      ($tuples :tuple2 [[1 2] [0 3]] false)
      [:var :tuple2 :hidden [:tuples :forbidden [[1 2] [0 3]]]]

      ($tuples [:tuple 1] [[8 9] [6 7]] false)
      [:var [:tuple 1] :hidden [:tuples :forbidden [[8 9] [6 7]]]]

      ($tuples :forbidden2 [[8 9] [6 7]] :forbidden)
      [:var :forbidden2 :hidden [:tuples :forbidden [[8 9] [6 7]]]]

      ($tuples-allowed :allowed [[8 9] [6 7]])
      [:var :allowed :hidden [:tuples :allowed [[8 9] [6 7]]]]

      ($tuples-forbidden :forbidden [[8 9] [6 7]])
      [:var :forbidden :hidden [:tuples :forbidden [[8 9] [6 7]]]]
      )
    )

  (testing "model/compile"
    (are [in out] (= out (model/compile in))
      [($tuples :tuple1 [[1 2] [0 3]])]
      [[:var :tuple1 :hidden [:tuples :allowed [[1 2] [0 3]]]]]

      [($tuples :tuple2 [[1 2] [0 3]] false)]
      [[:var :tuple2 :hidden [:tuples :forbidden [[1 2] [0 3]]]]]

      [($tuples [:tuple 1] [[8 9] [6 7]] false)]
      [[:var "[:tuple 1]" :hidden [:tuples :forbidden [[8 9] [6 7]]]]]
      )
    )

  (testing "compiler/compile"
    (choco-vars-string-assert
     '("Allowed tuples: {[1, 2][3, 4]}"
       "Fordidden tuples: {[5, 6][7, 8]}"
       "Fordidden tuples: {[9, 8][7, 6]}")
     [
      ($tuples :tuple1 [[1 2] [3 4]])
      ($tuples :tuple2 [[5 6] [7 8]] false)
      ($tuples-forbidden [:whatever 2] [[9 8] [7 6]])
      ])
    )

  (testing "solutions"
    (are [input expected] (= expected (solver/solutions input))
      [($in :a 0 1000)
       ($in :b 0 1000)
       ($tuples :tuples [[1 2] [3 4]])
       ($table [:a :b] :tuples)
       ]
      '({:a 1, :b 2}
        {:a 3, :b 4})
      )
    )
  )
