(ns ^:model loco.model.basics
  (:require [loco.model :as model])
  (:use clojure.test
        loco.core
        loco.constraints))

(deftest basics-test
  (is
   (=
    [[:var :x :public [:int 1 5]]
     [:var :y :public [:int 1 5]]
     [:var :z :public [:int 1 5]]
     [:var :2 :hidden [:const 2]]
     [:constraint [:all-equal [:z :2]]]
     [:constraint [:arithm [:x :< :y]]]
     [:constraint [:arithm [:y :<= :z]]]
     [:constraint [:arithm [:y :> :x]]]
     [:constraint [:arithm [:z :>= :y]]]
     [:constraint [:not-all-equal [:x :y]]]
     [:constraint [:not-all-equal [:x :y :z]]]]
    (->>
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
     model/compile)))

  (is
   (=
    [[:var :i :public [:int 0 5]]]
    (->> [($in :i 0 5)]
         model/compile)))

  (is
   (=
    [[:var :i :public [:int 0 5]]
     [:var :1 :hidden [:const 1]]
     [:constraint [:all-equal [:1 :i]]]]
    (->> [($in :i 0 5)
          ($= 1 :i)]
         model/compile)))

  (is
   (=
    [[:var :i :public [:int 0 5]]
     [:var :-i :proto [:int -5 0]]
     [:var :1 :hidden [:const 1]]
     [:constraint [:all-equal [:1 :-i]]]]
    (->> [($in :i 0 5)
          ($neg :-i :i)
          ($= 1 :-i)]
         model/compile)))

  (is
   (=
    [[:var :i :public [:int 0 5]]
     [:var :1 :hidden [:const 1]]
     [:var :-i :proto [:int -5 0]]
     [:constraint [:all-equal [:1 :-i]]]]
    (->> [($in :i 0 5)
          ($= 1 ($neg :i))]
         model/compile)))

  (is
   (=
    [[:var :i :public [:int 0 5]]
     [:var :-i :proto [:int -5 0]]
     [:var :1 :hidden [:const 1]]
     [:constraint [:all-equal [:1 :-i]]]]
    (->> [($in :i 0 5)
          ($neg :-i :i)
          ($= 1 :-i)]
         model/compile))))
