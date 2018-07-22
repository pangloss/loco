(ns loco.constraints.sum-test
  (:require [loco.model :as model]
            [loco.compiler :as compiler]
            [loco.solver :as solver]
            [loco.constraints.test-utils :as utils])
  (:use
   loco.constraints
   clojure.test))


      ;; [[:var :x :public [:int 0 5]]
      ;;  [:var :y :public [:int 0 5]]
      ;;  [:var :10 :hidden [:const 10]]
      ;;  [:var :5 :hidden [:const 5]]
      ;;  [:constraint ['sum [:10 '= [:x :y :5]]]]]
      ;; [($in :x 0 5)
      ;;  ($in :y 0 5)
      ;;  ($= 10 ($- :y :x 5))]

      ;;TODO: maybe it's better to make a boolean namespace instead of doing crazy overloading
      ;;not converted, not sure if it should be...
      ;; [[:var :x :public [:bool 0 1]]
      ;;  [:var :y :public [:bool 0 1]]
      ;;  [:var :z :public [:bool 0 1]]
      ;;  [:var :1 :hidden [:const 1]]
      ;;  [:constraint ['sum [:1 '= [:x :y :z]]]]]
      ;; [($bool :x)
      ;;  ($bool :y)
      ;;  ($bool :z)
      ;;  ($sum 1 := [:x :y :z])]

      ;;TODO: maybe it's better to put this into a set namespace, and not do crazy overloading like this. it would make for better documentation, at the least... maybe
      ;; [($in :sum 0 10)
      ;;  ($set :set [0 1 2] [0 1 2 3 4 5 6 7])
      ;;  ($sum :sum :set)]


(deftest ^:model sum-model-test
  (are [expected input] (= expected (->> input model/compile))
    [[:var :x :public [:int 0 5]]
     [:var :y :public [:int 0 5]]
     [:var :10 :hidden [:const 10]]
     [:var :5 :hidden [:const 5]]
     [:constraint ['sum [:10 '= [:x :y :5]]]]
     [:constraint ['sum [:10 '> [:x :y :5]]]]
     [:constraint ['sum [:10 '< [:x :y :5]]]]
     [:constraint ['sum [:10 '>= [:x :y :5]]]]
     [:constraint ['sum [:10 '<= [:x :y :5]]]]
     [:constraint ['sum [:10 '!= [:x :y :5]]]]]
   [($in :x 0 5)
    ($in :y 0 5)
    ($sum 10 := [:x :y 5])
    ($sum 10 :> [:x :y 5])
    ($sum 10 :< [:x :y 5])
    ($sum 10 :>= [:x :y 5])
    ($sum 10 :<= [:x :y 5])
    ($sum 10 :!= [:x :y 5])]

   [[:var :x :public [:bool 0 1]]
    [:var :y :public [:bool 0 1]]
    [:var :z :public [:bool 0 1]]
    [:var :1 :hidden [:const 1]]
    [:constraint ['sum [:1 '= [:x :y :z]]]]]
   [($bool :x)
    ($bool :y)
    ($bool :z)
    ($sum 1 := [:x :y :z])]

   [[:var :sum :public [:int 0 10]]
    [:var :set :public [:set #{0 1 2} #{0 7 1 4 6 3 2 5}]]
    [:constraint ['sum [:sum '= :set]]]]
   [($in :sum 0 10)
    ($set :set [0 1 2] [0 1 2 3 4 5 6 7])
    ($sum :sum :set)])
  )

(deftest ^:compiler sum-compile-test
  (are [expected input] (= expected (utils/constraints-strings input))
    '("SUM ([z + y + x = 1])")
    [($bool :x)
     ($bool :y)
     ($bool :z)
     ($sum 1 := [:x :y :z])]

    '("SUM ([z + y + x = 2])")
    [($in :x 1 2)
     ($in :y 1 2)
     ($in :z 1 2)
     ($sum 2 := [:x :y :z])]

    '("SUM ([z + y + x = 2])")
    [($in :x 1 2)
     ($bool :y)
     ($in :z 1 2)
     ($sum 2 := [:x :y :z])]

    '("SETSUM ([PropSumOfElements(set, sum)])")
    [($in :sum 0 10)
     ($set :set [0 1 2] [0 1 2 3 4 5 6 7])
     ($sum :sum :set)]
    )
  )

(deftest ^:solutions sum-solution-test
  (are [expected input] (= expected (->> input solver/solutions))
    '({:x 0, :y 1, :z 0}
      {:x 0, :y 0, :z 1}
      {:x 1, :y 0, :z 0})
    [($bool :x)
     ($bool :y)
     ($bool :z)
     ($sum 1 := [:x :y :z])]

    '({:x 1, :y 1, :z 1})
    [($in :x 1 2)
     ($in :y 1 2)
     ($in :z 1 2)
     ($sum 3 := [:x :y :z])]

    '({:x 1, :y 0, :z 1})
    [($in :x 1 2)
     ($bool :y)
     ($in :z 1 2)
     ($sum 2 := [:x :y :z])]

    '({:sum 3,  :set #{0 1 2}}
      {:sum 6,  :set #{0 1 3 2}}
      {:sum 7,  :set #{0 1 4 2}}
      {:sum 8,  :set #{0 1 2 5}}
      {:sum 9,  :set #{0 1 6 2}}
      {:sum 10, :set #{0 1 4 3 2}}
      {:sum 10, :set #{0 7 1 2}})
    [($in :sum 0 10)
     ($set :set [0 1 2] [0 1 2 3 4 5 6 7])
     ($sum :sum :set)]
    )
  )
