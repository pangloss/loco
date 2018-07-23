(ns loco.constraints.vars-test
  (:require
   [clojure.test :refer :all]
   [loco.constraints :refer :all]
   [loco.constraints.test-utils :refer :all]
   )  
  (:import org.chocosolver.solver.Model))

(deftest const-vars-test
  (is
   (loco?
    [($const :7 7)
     ($const :a 1)               
     ($const [:b 10] 2)          
     ($const [:constraint 10] 2) 
     ($const- [:constraint 20] 4)
     ($const [:_constraint 30] 4)]
    {:identity [[:var :7 :public [:const 7]]
                [:var :a :public [:const 1]]
                [:var [:b 10] :public [:const 2]]
                [:var [:constraint 10] :public [:const 2]]
                [:var [:constraint 20] :hidden [:const 4]]
                [:var [:_constraint 30] :hidden [:const 4]]]
     :compiled [["7 = 7"
                 "a = 1"
                 "[:b 10] = 2"
                 "[:constraint 10] = 2"
                 "[:constraint 20] = 4"
                 "[:_constraint 30] = 4"]
                []]
     :solutions #{{:7 7,
                   :a 1,
                   [:b 10] 2,
                   [:constraint 10] 2}}
     }))
  )

(deftest bool-vars-test
  (is
   (loco?
    [
     ($bool :8)               
     ($bool [:constraint 11])
     ($bool- [:constraint 111]) 
     ($bool [:_constraint 112])
     ($bools :a :_b)           
     ($bools [:a 1] [:_b 2])]
    {:identity [[:var :8 :public [:bool 0 1]]
                [:var [:constraint 11] :public [:bool 0 1]]
                [:var [:constraint 111] :hidden [:bool 0 1]]
                [:var [:_constraint 112] :hidden [:bool 0 1]]
                [[:var :a :public [:bool 0 1]]
                 [:var :_b :hidden [:bool 0 1]]]
                [[:var [:a 1] :public [:bool 0 1]]
                 [:var [:_b 2] :hidden [:bool 0 1]]]]
     :compiled [["8 = [0,1]"
                 "[:constraint 11] = [0,1]"
                 "[:constraint 111] = [0,1]"
                 "[:_constraint 112] = [0,1]"
                 "a = [0,1]"
                 "_b = [0,1]"
                 "[:a 1] = [0,1]"
                 "[:_b 2] = [0,1]"]
                []]
     :solutions #{{:8 0, [:constraint 11] 0, :a 0, [:a 1] 1}
                  {:8 0, [:constraint 11] 1, :a 1, [:a 1] 0}
                  {:8 0, [:constraint 11] 1, :a 1, [:a 1] 1}
                  {:8 0, [:constraint 11] 1, :a 0, [:a 1] 0}
                  {:8 1, [:constraint 11] 0, :a 1, [:a 1] 0}
                  {:8 0, [:constraint 11] 0, :a 1, [:a 1] 0}
                  {:8 1, [:constraint 11] 1, :a 1, [:a 1] 0}
                  {:8 0, [:constraint 11] 0, :a 1, [:a 1] 1}
                  {:8 1, [:constraint 11] 1, :a 0, [:a 1] 0}
                  {:8 1, [:constraint 11] 1, :a 0, [:a 1] 1}
                  {:8 1, [:constraint 11] 0, :a 1, [:a 1] 1}
                  {:8 0, [:constraint 11] 1, :a 0, [:a 1] 1}
                  {:8 1, [:constraint 11] 0, :a 0, [:a 1] 0}
                  {:8 0, [:constraint 11] 0, :a 0, [:a 1] 0}
                  {:8 1, [:constraint 11] 0, :a 0, [:a 1] 1}
                  {:8 1, [:constraint 11] 1, :a 1, [:a 1] 1}}
     }))
  )

(deftest int-vars-test
  (is
   (loco?
    [
     ($in :a 1)
     ($in :b 2 2) 
     ($in :c 3 4) 
     ($in [:constraint 12] 5) 
     ($in :d [6 9])       
     ($in :f 10 15 :bounded)  
     ($in :_g 10 20 :bounded) 
     ($in :_d [6 7 8 9])      
     ($in [:_constraint 12] 5)
     ]
    {:identity [[:var :a :public [:int 1 1]]
                [:var :b :public [:int 2 2]]
                [:var :c :public [:int 3 4]]
                [:var [:constraint 12] :public [:int 5 5]]
                [:var :d :public [:int [6 9]]]
                [:var :f :public [:int 10 15 :bounded]]
                [:var :_g :hidden [:int 10 20 :bounded]]
                [:var :_d :hidden [:int [6 7 8 9]]]
                [:var [:_constraint 12] :hidden [:int 5 5]]]
     :compiled [["a = 1"
                 "b = 2"
                 "c = {3..4}"
                 "[:constraint 12] = 5"
                 "d = {6,9}"
                 "f = [10,15]"
                 "_g = [10,20]"
                 "_d = {6..9}"
                 "[:_constraint 12] = 5"]
                []]
     :solutions #{{:a 1, :b 2, :c 3, [:constraint 12] 5, :d 9, :f 14}
                  {:a 1, :b 2, :c 3, [:constraint 12] 5, :d 6, :f 12}
                  {:a 1, :b 2, :c 4, [:constraint 12] 5, :d 9, :f 10}
                  {:a 1, :b 2, :c 4, [:constraint 12] 5, :d 6, :f 11}
                  {:a 1, :b 2, :c 3, [:constraint 12] 5, :d 9, :f 12}
                  {:a 1, :b 2, :c 3, [:constraint 12] 5, :d 9, :f 13}
                  {:a 1, :b 2, :c 4, [:constraint 12] 5, :d 6, :f 14}
                  {:a 1, :b 2, :c 4, [:constraint 12] 5, :d 9, :f 11}
                  {:a 1, :b 2, :c 4, [:constraint 12] 5, :d 9, :f 14}
                  {:a 1, :b 2, :c 4, [:constraint 12] 5, :d 9, :f 15}
                  {:a 1, :b 2, :c 3, [:constraint 12] 5, :d 6, :f 14}
                  {:a 1, :b 2, :c 3, [:constraint 12] 5, :d 9, :f 11}
                  {:a 1, :b 2, :c 3, [:constraint 12] 5, :d 9, :f 15}
                  {:a 1, :b 2, :c 3, [:constraint 12] 5, :d 6, :f 15}
                  {:a 1, :b 2, :c 3, [:constraint 12] 5, :d 6, :f 11}
                  {:a 1, :b 2, :c 4, [:constraint 12] 5, :d 6, :f 13}
                  {:a 1, :b 2, :c 3, [:constraint 12] 5, :d 6, :f 10}
                  {:a 1, :b 2, :c 4, [:constraint 12] 5, :d 6, :f 12}
                  {:a 1, :b 2, :c 4, [:constraint 12] 5, :d 9, :f 12}
                  {:a 1, :b 2, :c 3, [:constraint 12] 5, :d 9, :f 10}
                  {:a 1, :b 2, :c 4, [:constraint 12] 5, :d 9, :f 13}
                  {:a 1, :b 2, :c 3, [:constraint 12] 5, :d 6, :f 13}
                  {:a 1, :b 2, :c 4, [:constraint 12] 5, :d 6, :f 10}
                  {:a 1, :b 2, :c 4, [:constraint 12] 5, :d 6, :f 15}}
     }))
  )

(deftest set-vars-test
  (is
   (loco?
    [
     ($set :a [] [])       
     ($set :b [] [1 2 3])  
     ($set :d [1] [1 2 3]) 
     ($set :e [1 2 3])     
     ($set [:constraint 2] [1 2 3]) 
     ($set [:_constraint 2] [1 2 3]) 
     ($set :_e [1 2 3])     
     ]
    {:identity [[:var :a :public [:set #{} #{}]]
                [:var :b :public [:set #{} #{1 2 3}]]
                [:var :d :public [:set #{1} #{1 2 3}]]
                [:var :e :public [:set #{1 2 3}]]
                [:var [:constraint 2] :public [:set #{1 2 3}]]
                [:var [:_constraint 2] :hidden [:set #{1 2 3}]]
                [:var :_e :hidden [:set #{1 2 3}]]]
     :compiled [["a = {}"
                 "b = [{}, {1, 2, 3}]"
                 "d = [{1}, {1, 2, 3}]"
                 "e = {1, 2, 3}"
                 "[:constraint 2] = {1, 2, 3}"
                 "[:_constraint 2] = {1, 2, 3}"
                 "_e = {1, 2, 3}"]
                []]
     :solutions #{{:a #{},:b #{3 2},:d #{1 3},:e #{1 3 2},[:constraint 2] #{1 3 2}}
                  {:a #{},:b #{1 3},:d #{1},:e #{1 3 2},[:constraint 2] #{1 3 2}}
                  {:a #{},:b #{},:d #{1 3},:e #{1 3 2},[:constraint 2] #{1 3 2}}
                  {:a #{},:b #{3 2},:d #{1 3 2},:e #{1 3 2},[:constraint 2] #{1 3 2}}
                  {:a #{},:b #{},:d #{1},:e #{1 3 2},[:constraint 2] #{1 3 2}}
                  {:a #{},:b #{1 3 2},:d #{1},:e #{1 3 2},[:constraint 2] #{1 3 2}}
                  {:a #{},:b #{1 3 2},:d #{1 2},:e #{1 3 2},[:constraint 2] #{1 3 2}}
                  {:a #{},:b #{1 3 2},:d #{1 3},:e #{1 3 2},[:constraint 2] #{1 3 2}}
                  {:a #{},:b #{1 2},:d #{1 3},:e #{1 3 2},[:constraint 2] #{1 3 2}}
                  {:a #{},:b #{3},:d #{1 3},:e #{1 3 2},[:constraint 2] #{1 3 2}}
                  {:a #{},:b #{1},:d #{1 2},:e #{1 3 2},[:constraint 2] #{1 3 2}}
                  {:a #{},:b #{1 3 2},:d #{1 3 2},:e #{1 3 2},[:constraint 2] #{1 3 2}}
                  {:a #{},:b #{1},:d #{1},:e #{1 3 2},[:constraint 2] #{1 3 2}}
                  {:a #{},:b #{2},:d #{1 2},:e #{1 3 2},[:constraint 2] #{1 3 2}}
                  {:a #{},:b #{3 2},:d #{1},:e #{1 3 2},[:constraint 2] #{1 3 2}}
                  {:a #{},:b #{1 2},:d #{1 2},:e #{1 3 2},[:constraint 2] #{1 3 2}}
                  {:a #{},:b #{1 2},:d #{1 3 2},:e #{1 3 2},[:constraint 2] #{1 3 2}}
                  {:a #{},:b #{2},:d #{1},:e #{1 3 2},[:constraint 2] #{1 3 2}}
                  {:a #{},:b #{3},:d #{1 2},:e #{1 3 2},[:constraint 2] #{1 3 2}}
                  {:a #{},:b #{3},:d #{1 3 2},:e #{1 3 2},[:constraint 2] #{1 3 2}}
                  {:a #{},:b #{3},:d #{1},:e #{1 3 2},[:constraint 2] #{1 3 2}}
                  {:a #{},:b #{1},:d #{1 3},:e #{1 3 2},[:constraint 2] #{1 3 2}}
                  {:a #{},:b #{1 3},:d #{1 3 2},:e #{1 3 2},[:constraint 2] #{1 3 2}}
                  {:a #{},:b #{1 3},:d #{1 3},:e #{1 3 2},[:constraint 2] #{1 3 2}}
                  {:a #{},:b #{1},:d #{1 3 2},:e #{1 3 2},[:constraint 2] #{1 3 2}}
                  {:a #{},:b #{},:d #{1 3 2},:e #{1 3 2},[:constraint 2] #{1 3 2}}
                  {:a #{},:b #{2},:d #{1 3 2},:e #{1 3 2},[:constraint 2] #{1 3 2}}
                  {:a #{},:b #{2},:d #{1 3},:e #{1 3 2},[:constraint 2] #{1 3 2}}
                  {:a #{},:b #{1 3},:d #{1 2},:e #{1 3 2},[:constraint 2] #{1 3 2}}
                  {:a #{},:b #{},:d #{1 2},:e #{1 3 2},[:constraint 2] #{1 3 2}}
                  {:a #{},:b #{3 2},:d #{1 2},:e #{1 3 2},[:constraint 2] #{1 3 2}}
                  {:a #{},:b #{1 2},:d #{1},:e #{1 3 2},[:constraint 2] #{1 3 2}}}
     }))
  )

(deftest task-vars-test
  (is
   (loco?
    [($in :a 1 2)
     ($in :b 2 4)
     ($in :c 3 5)
     ($task :my-task [1 2] [2 3] [3 4]) 
     ($task :my-task :a :b :c) 
     ($task [:my-task 1] :a :b :c)
     ($task :_my-task [1 2] [2 3] [3 4])
     ($task [:_my-task 1] :a :b :c) ]
    {:identity [[:var :a :public [:int 1 2]]
                [:var :b :public [:int 2 4]]
                [:var :c :public [:int 3 5]]
                [:var :my-task :public [:task [1 2] [2 3] [3 4]]]
                [:var :my-task :public [:task :a :b :c]]
                [:var [:my-task 1] :public [:task :a :b :c]]
                [:var :_my-task :hidden [:task [1 2] [2 3] [3 4]]]
                [:var [:_my-task 1] :hidden [:task :a :b :c]]]
     :compiled [["a = {1..2}"
                 "b = {2..4}"
                 "c = {3..5}"
                 "Task[start=IV_1 = {1..2}, duration=IV_2 = {2..3}, end=IV_3 = {3..4}]"
                 "Task[start=a = {1..2}, duration=b = {2..4}, end=c = {3..5}]"
                 "Task[start=a = {1..2}, duration=b = {2..4}, end=c = {3..5}]"
                 "Task[start=IV_4 = {1..2}, duration=IV_5 = {2..3}, end=IV_6 = {3..4}]"
                 "Task[start=a = {1..2}, duration=b = {2..4}, end=c = {3..5}]"]
                []]
     :solutions #{{:a 1,:b 4,:c 5,:my-task {:start 1, :duration 4, :end 5},[:my-task 1] {:start 1, :duration 4, :end 5}}
                  {:a 1,:b 3,:c 4,:my-task {:start 1, :duration 3, :end 4},[:my-task 1] {:start 1, :duration 3, :end 4}}
                  {:a 2,:b 3,:c 5,:my-task {:start 2, :duration 3, :end 5},[:my-task 1] {:start 2, :duration 3, :end 5}}
                  {:a 2,:b 2,:c 4,:my-task {:start 2, :duration 2, :end 4},[:my-task 1] {:start 2, :duration 2, :end 4}}
                  {:a 1,:b 2,:c 3,:my-task {:start 1, :duration 2, :end 3},[:my-task 1] {:start 1, :duration 2, :end 3}}}
     }))
  
  
  )

(deftest tuples-test
  (is
   (loco?
    [($tuples :tuple1 [[1 2] [0 3]])
     ($tuples :tuple2 [[1 2] [0 3]] false)
     ($tuples [:tuple 1] [[8 9] [6 7]] false)
     ($tuples :forbidden2 [[8 9] [6 7]] :forbidden)
     ($tuples-allowed :allowed [[8 9] [6 7]])
     ($tuples-forbidden :forbidden [[8 9] [6 7]])
     ]
    {:identity [[:var :tuple1 :hidden [:tuples :allowed [[1 2] [0 3]]]]
                [:var :tuple2 :hidden [:tuples :forbidden [[1 2] [0 3]]]]
                [:var [:tuple 1] :hidden [:tuples :forbidden [[8 9] [6 7]]]]
                [:var :forbidden2 :hidden [:tuples :forbidden [[8 9] [6 7]]]]
                [:var :allowed :hidden [:tuples :allowed [[8 9] [6 7]]]]
                [:var :forbidden :hidden [:tuples :forbidden [[8 9] [6 7]]]]],
     :compiled [["Allowed tuples: {[1, 2][0, 3]}"
                 "Fordidden tuples: {[1, 2][0, 3]}"
                 "Fordidden tuples: {[8, 9][6, 7]}"
                 "Fordidden tuples: {[8, 9][6, 7]}"
                 "Allowed tuples: {[8, 9][6, 7]}"
                 "Fordidden tuples: {[8, 9][6, 7]}"]
                []]}
    ))

  (is
   (loco?
    [($in :a 0 1000)
     ($in :b 0 1000)
     ($tuples :tuples [[1 2] [3 4]])
     ($table [:a :b] :tuples)]
    {:solutions #{{:a 3, :b 4} {:a 1, :b 2}}}
    ))
  )
