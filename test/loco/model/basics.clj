(ns loco.model.basics
  (:require [loco.model :as model])
  (:use clojure.test
        loco.core
        loco.constraints))

(deftest basics-test
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
