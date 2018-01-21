(ns loco.solver.vars-test
  (:use clojure.test
        loco.constraints)
  (:require
   [loco.solver :as solver]
   [loco.model :as model]))

(deftest solutions-var-test
  (testing "task-vars"
    (are [input expected] (= expected (solver/solutions input))
      [($task :task [0 1] [0 1] [0 1])] '({:task {:start 0, :duration 0, :end 0}}
                                          {:task {:start 0, :duration 1, :end 1}}
                                          {:task {:start 1, :duration 0, :end 1}})

      [($int- :start [0 1])
       ($int- :duration 0)
       ($int- :end [0 1])
       ($task :task :start :duration :end)] '({:task {:start 0, :duration 0, :end 0}}
                                              {:task {:start 1, :duration 0, :end 1}})
      [($int- :start [0 1])
       ($int- :end [0 1])
       ($task :task :start [1] :end)] '({:task {:start 0, :duration 1, :end 1}})

      [($int- :start [0 1])
       ($int- :end [0 3 5])
       ($task :task :start [[1 3 5]] :end)] '({:task {:start 0, :duration 3, :end 3}}
                                              {:task {:start 0, :duration 5, :end 5}})
      )
    )

  (testing "set-vars"
    (are [input expected] (= expected (solver/solutions input))
      [($set :a [] [])]  '({:a #{}})
      [($set :a [] [0])] '({:a #{0}} {:a #{}})
      [($set :a [0] [0])] '({:a #{0}})
      [($set :a [0] [0 1])] '({:a #{0 1}} {:a #{0}})
      )
    )

  (testing "bool-vars"
    (are [input expected] (= expected (solver/solutions input))
      [($bool :a)]     '({:a 0} {:a 1})
      [($bool [:a 1])] '({[:a 1] 0} {[:a 1] 1})
      [($bool- :a)]    '({} {})
      [($bools :a :b)] '({:a 0, :b 0} {:a 1, :b 0} {:a 0, :b 1} {:a 1, :b 1})
      )
    )

  (testing "int-vars"
    (are [input expected] (= expected (solver/solutions input))
      [($const :a 0)]     '({:a 0})
      [($const- :a 0)]    '({})
      [($const- :aa 0)
       ($const :a 0)]     '({:a 0})
      [($int :a 0)]       '({:a 0})
      [($int :a 0 4)]     '({:a 0} {:a 1} {:a 2} {:a 3} {:a 4})
      [($int [:a 1] 0 2)] '({[:a 1] 0} {[:a 1] 1} {[:a 1] 2})
      )
    )
  )
