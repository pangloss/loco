(ns loco.constraints.sum-test
  (:require [loco.model :as model]
            [loco.compiler :as compiler]
            [loco.solver :as solver])
  (:use
   loco.constraints
   loco.constraints.sum
   clojure.test))

(deftest sum-model-test
  (are [expected input] (= expected (->> input model/compile))
    [[:var :x :public [:int 0 5]]
     [:var :y :public [:int 0 5]]
     [:var :10 :hidden [:const 10]]
     [:var :5 :hidden [:const 5]]
     [:constraint [:con/sum [:10 :op/= [:x :y :5]]]]
     [:constraint [:con/sum [:10 :op/> [:x :y :5]]]]
     [:constraint [:con/sum [:10 :op/< [:x :y :5]]]]
     [:constraint [:con/sum [:10 :op/>= [:x :y :5]]]]
     [:constraint [:con/sum [:10 :op/<= [:x :y :5]]]]
     [:constraint [:con/sum [:10 :op/!= [:x :y :5]]]]]
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
    [:constraint [:con/sum [:1 :op/= [:x :y :z]]]]]
   [($bool :x)
    ($bool :y)
    ($bool :z)
    ($sum 1 := [:x :y :z])]

   [[:var :sum :public [:int 0 10]]
    [:var :set :public [:set #{0 1 2} #{0 7 1 4 6 3 2 5}]]
    [:constraint [:con/sum [:sum :op/= :set]]]]
   [($in :sum 0 10)
    ($set :set [0 1 2] [0 1 2 3 4 5 6 7])
    ($sum :sum :set)])
  )



(defn constraints-strings [input]
  (->> input
       model/compile
       compiler/compile
       :model
       .getCstrs
       (map str)))

(deftest sum-compile-test
  (are [expected input] (= expected (constraints-strings input))
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

(defn solutions [input]
  (->> input solver/solutions))

(deftest sum-solution-test
  (are [expected input] (= expected (->> input solutions))
    '({:x 0, :y 1, :z 0} {:x 0, :y 0, :z 1} {:x 1, :y 0, :z 0})
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

    '({:sum 3, :set #{0 1 2}}
      {:sum 6, :set #{0 1 3 2}}
      {:sum 7, :set #{0 1 4 2}}
      {:sum 8, :set #{0 1 2 5}}
      {:sum 9, :set #{0 1 6 2}}
      {:sum 10, :set #{0 1 4 3 2}}
      {:sum 10, :set #{0 7 1 2}})
    [($in :sum 0 10)
     ($set :set [0 1 2] [0 1 2 3 4 5 6 7])
     ($sum :sum :set)]
    )
  )

(deftest sum-spec
  (try
    (->>
     [($in   :r 0 10)
      ($in   :x 1 2)
      ($bool :y)
      ($set  :z [] [1 2])
      ($sum :r := [:x :y :z])]
     solver/solutions)

    (catch RuntimeException e
      (is
       (=
        #:clojure.spec.alpha{:problems
                             '({:path [:args :set :var],
                                :pred loco.constraints.sum/set-var?,
                                :val
                                ["x = {1..2}"
                                 "y = [0,1]"
                                 "z = [{}, {1, 2}]"],
                                :via [:loco.constraints.sum/sum],
                                :in [1 2]}
                               {:path [:args :bools :vars],
                                :pred bool-var?,
                                :val "x = {1..2}",
                                :via
                                [:loco.constraints.sum/sum
                                 :loco.constraints.sum/bools
                                 :loco.constraints.sum/bools],
                                :in [1 2 0]}
                               {:path [:args :bools :vars],
                                :pred bool-var?,
                                :val "z = [{}, {1, 2}]",
                                :via
                                [:loco.constraints.sum/sum
                                 :loco.constraints.sum/bools
                                 :loco.constraints.sum/bools],
                                :in [1 2 2]}
                               {:path [:args :ints :vars],
                                :pred int-var?,
                                :val "z = [{}, {1, 2}]",
                                :via
                                [:loco.constraints.sum/sum
                                 :loco.constraints.sum/ints
                                 :loco.constraints.sum/ints],
                                :in [1 2 2]}),
                             :spec :loco.constraints.sum/sum,
                             :value
                             [:con/sum
                              ["r = {0..10}"
                               :op/=
                               ["x = {1..2}"
                                "y = [0,1]"
                                "z = [{}, {1, 2}]"]]]}
        (.getData e)))))

  )
