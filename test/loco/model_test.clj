(ns loco.model-test
  (:require [loco.model :as model])
  (:use clojure.test
        loco.core
        loco.constraints))

(use 'clojure.pprint)

(is
 (= [[:var :i :public [:int 0 5]]]
    (->> [($in :i 0 5)]
         model/compile)))

(is
 (= [[:var :i :public [:int 0 5]]
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
       model/compile)))

(is
 (=
  [[:var :_a :hidden [:const 1]]
   [:var :x :public [:int 0 5]]
   [:var :y :public [:int 0 5]]
   [:var :5 :hidden [:const 5]]
   [:var :-y :proto [:int -5 0]]
   [:var :x-y :proto [:int -5 5]]
   [:var :-x :proto [:int -5 0]]
   [:var :y-x :proto [:int -5 5]]
   [:constraint [:sum [:x-y := :x :-y]]]
   [:constraint [:sum [:y-x := :y :-x]]]
   [:constraint [:all-equal [:5 :x-y :y-x]]]]
    (->> [($in :_a 1)
          ($in :x 0 5)
          ($in :y 0 5)
          ($= 5 ($- :x :y) ($- :y :x))]
         model/compile)))

(is
 (=
  [[:var :x :public [:int 0 5]]
   [:var :y :public [:int 0 5]]
   [:var :5 :hidden [:const 5]]
   [:var :-x :proto [:int -5 0]]
   [:var :y-x :proto [:int -5 5]]
   [:var :-y-x :proto [:int -5 5]]
   [:var :x-y-x :proto [:int -5 10]]
   [:constraint [:sum [:y-x := :y :-x]]]
   [:constraint [:sum [:x-y-x := :x :-y-x]]]
   [:constraint [:all-equal [:5 :x-y-x]]]]
  (->> [($in :x 0 5)
        ($in :y 0 5)
        ($= 5 ($- :x ($- :y :x)))]
       model/compile)))

(is
 (=
  [[:var :x :public [:int 0 5]]
   [:var :y :public [:int 0 5]]
   [:var :5 :hidden [:const 5]]
   [:var :3 :hidden [:const 3]]
   [:var :7 :hidden [:const 7]]
   [:var :-x :proto [:int -5 0]]
   [:var :-7 :proto [:const -7]]
   [:var :y-x-7 :proto [:int -12 -2]]
   [:var :-3 :proto [:const -3]]
   [:var :-y-x-7 :proto [:int 2 12]]
   [:var :x-3-y-x-7 :proto [:int -1 14]]
   [:constraint [:sum [:y-x-7 := :y :-x :-7]]]
   [:constraint [:sum [:x-3-y-x-7 := :x :-3 :-y-x-7]]]
   [:constraint [:all-equal [:5 :x-3-y-x-7]]]]
    (->> [($in :x 0 5)
          ($in :y 0 5)
          ($= 5 ($- :x 3 ($- :y :x 7)))]
         model/compile)))

(is
 (=
  [[:var :x :public [:int 0 5]]
   [:var :y :public [:int 0 5]]
   [:var :10 :hidden [:const 10]]
   [:var :5 :hidden [:const 5]]
   [:var :x+y+5 :proto [:int 5 15]]
   [:constraint [:sum [:x+y+5 := :x :y :5]]]
   [:constraint [:all-equal [:10 :x+y+5]]]]
  (->> [($in :x 0 5)
        ($in :y 0 5)
        ($= 10 ($+ :x :y 5))] ;;maybe this is a proto-constraint
       model/compile
       )))

(is
 (=
  [[:var :x :public [:int 0 5]]
   [:var :y :public [:int 0 5]]
   [:var :0 :hidden [:const 0]]
   [:var :x*y :proto [:int 0 25]]
   [:constraint [:all-equal [:0 :x*y]]]]
  (->> [($in :x 0 5)
        ($in :y 0 5)
        ($= 0 ($* :x :y))]
       model/compile)))

(is
 (=
  [[:var :x :public [:int -5 5]]
   [:var :y :public [:int 0 5]]
   [:var :0 :hidden [:const 0]]
   [:var :x*y :proto [:int -25 25]]
   [:constraint [:all-equal [:0 :x*y]]]]
  (->> [($in :x -5 5)
        ($in :y 0 5)
        ($= 0 ($* :x :y))]
       model/compile)))

(is
 (= [[:var :x :public [:int -5 5]]
     [:var :y :public [:int 0 2]]
     [:var :0 :hidden [:const 0]]
     [:var :x*y :proto [:int -25 10]]
     [:var :-x*y :proto [:int -10 25]]
     [:constraint [:all-equal [:0 :-x*y]]]]
    (->> [($in :x -5 5)
          ($in :y 0 2)
          ($= 0 ($neg ($* :x :y)))]
         model/compile)))

(->> [($in :x 0 100)
      ($in :y 0 10)
      ($arithm :y := :x :/ 10)]
     model/compile)
(=
 [[:var :x :public [:int 0 100]]
  [:var :y :public [:int 0 10]]
  [:var :10 :hidden [:const 10]]
  [:constraint [:arithm [:y := :x :/ :10]]]]
 (->> [($in :x 0 100)
       ($in :y 0 10)
       ($arithm :y := :x :/ 10)]
      model/compile))

(=
 [[:var :x :public [:int 0 100]]
  [:var :y :public [:int 0 10]]
  [:var :z :public [:int 0 5]]
  [:constraint [:arithm [:y := :x :/ :z]]]]
 (->> [($in :x 0 100)
       ($in :y 0 10)
       ($in :z 0 5)
       ($arithm :y := :x :/ :z)]
      model/compile))
