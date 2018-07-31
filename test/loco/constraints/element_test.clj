(ns loco.constraints.element-test
  (:require
   [clojure.test :refer :all]
   [loco.constraints :refer :all]
   [loco.constraints.test-utils :as utils]
   ))

(deftest element-test
  (is
   (loco?
    [($in :index 0 2)
     ($in :array-val 1 5)
     ($element :array-val [1 2 3 4 5] :index)]
    {:model
     '[[:var :index :public [:int 0 2]]
       [:var :array-val :public [:int 1 5]]
       [element
        [:array-val [in [1 2 3 4 5]] [at :index] [offset 0]]]],
     :compiled
     [["index = {0..2}" "array-val = {1..5}"]
      ["ELEMENT ([element(array-val = {1..5} =  <1, 2, 3, 4, 5> [index = {0..2}])])"]],
     :solutions
     #{
       {:index 0, :array-val 1}
       {:index 1, :array-val 2}
       {:index 2, :array-val 3}
       }}
    ))

  (is
   (loco?
    [($in :a [10 99])
     ($in :b [0 9])
     ($in :c [100 1000])
     ($in :index 0 2)
     ($in :array-val 0 1001)
     ($element :array-val [:a :b :c] :index 1)]
    {:compiled
     [["a = {10,99}"
       "b = {0,9}"
       "c = {100,1000}"
       "index = {0..2}"
       "array-val = {0..1001}"]
      ["ELEMENT ([PropElementV_fast(array-val, index, a, ..., c)])"]],
     :solutions
     #{
       {:a 99, :b 9, :c 100,  :index 2, :array-val 9}
       {:a 99, :b 9, :c 100,  :index 1, :array-val 99}
       {:a 10, :b 9, :c 1000, :index 1, :array-val 10}
       {:a 99, :b 0, :c 100,  :index 2, :array-val 0}
       {:a 99, :b 0, :c 100,  :index 1, :array-val 99}
       {:a 99, :b 0, :c 1000, :index 2, :array-val 0}
       {:a 10, :b 9, :c 1000, :index 2, :array-val 9}
       {:a 99, :b 0, :c 1000, :index 1, :array-val 99}
       {:a 10, :b 0, :c 100,  :index 1, :array-val 10}
       {:a 10, :b 0, :c 1000, :index 1, :array-val 10}
       {:a 10, :b 0, :c 100,  :index 2, :array-val 0}
       {:a 99, :b 9, :c 1000, :index 1, :array-val 99}
       {:a 10, :b 9, :c 100,  :index 1, :array-val 10}
       {:a 10, :b 9, :c 100,  :index 2, :array-val 9}
       {:a 10, :b 0, :c 1000, :index 2, :array-val 0}
       {:a 99, :b 9, :c 1000, :index 2, :array-val 9}
       }}
    "should not take on any values of :c due to offset behavior"
    ))

  (is
   (loco?
    [($in :index 0 2)
     ($set :array-val #{2 3 5} #{2 3 5 7 11 13 17 19 23 29 31 37})
     ($set :table1 #{} #{2 3 5 7})
     ($set :table2 #{} #{11 })
     ($set :table3 #{} #{23 })
     ($element :array-val [:table1 :table2 :table2] :index)]
    {:compiled
     [["index = {0..2}"
       "array-val = [{2, 3, 5}, {2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37}]"
       "table1 = [{}, {2, 3, 5, 7}]"
       "table2 = [{}, {11}]"
       "table3 = [{}, {23}]"]
      ["SETELEMENT ([PropElement(table1, table2, table2, ..., index), PropElement(table1, table2, table2, ..., index)])"]],
     :solutions
     #{
       {:index 0, :array-val #{3 2 5},  :table1 #{3 2 5},  :table2 #{11},:table3 #{23}}
       {:index 0, :array-val #{3 2 5},  :table1 #{3 2 5},  :table2 #{11},:table3 #{}}
       {:index 0, :array-val #{7 3 2 5},:table1 #{7 3 2 5},:table2 #{},  :table3 #{}}
       {:index 0, :array-val #{3 2 5},  :table1 #{3 2 5},  :table2 #{},  :table3 #{}}
       {:index 0, :array-val #{7 3 2 5},:table1 #{7 3 2 5},:table2 #{11},:table3 #{23}}
       {:index 0, :array-val #{3 2 5},  :table1 #{3 2 5},  :table2 #{},  :table3 #{23}}
       {:index 0, :array-val #{7 3 2 5},:table1 #{7 3 2 5},:table2 #{11},:table3 #{}}
       {:index 0, :array-val #{7 3 2 5},:table1 #{7 3 2 5},:table2 #{},  :table3 #{23}}
       }}
    ))

  )
