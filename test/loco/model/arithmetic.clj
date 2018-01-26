(ns ^:model loco.model.arithmetic
  (:require [loco.model :as model])
  (:use clojure.test
        loco.model.test
        loco.constraints))

(deftest $div-test
  (compiled-assert
   [[:var :x :public [:int 0 10]]
    [:var :y :public [:int 0 10]]
    [:var :z :public [:int 0 10]]
    [:constraint [:div [:z := :x :/ :y]]]]

   [($in :x 0 10)
    ($in :y 0 10)
    ($in :z 0 10)
    ($div :x :y :z)])
  )

(deftest $mod-test
  (compiled-assert
   [[:var :x :public [:int 0 100]]
    [:var :y :public [:int 0 10]]
    [:var :z :public [:int 0 5]]
    [:constraint [:mod [:z := :x :% :y]]]]

   [($in :x 0 100)
    ($in :y 0 10)
    ($in :z 0 5)
    ($mod :x :y :z)])

  (compiled-assert
   [[:var :x :public [:int 0 100]]
    [:var :y :public [:int 0 10]]
    [:var :z :public [:int 0 5]]
    [:var :x%y :proto [:int 0 10]]
    [:constraint [:mod [:x%y := :x :% :y]]]
    [:constraint ['arithm [:z '= :x%y]]]]

   [($in :x 0 100)
    ($in :y 0 10)
    ($in :z 0 5)
    ($= :z ($mod :x :y))])
  )

(deftest abs-test
  (compiled-assert
   [[:var :y :public [:int 0 10]]
    [:var :z :public [:int -5 0]]
    [:constraint [:abs [:y := :z]]]]

   [($in :y 0 10)
    ($in :z -5 0)
    ($abs :y :z)])

  (compiled-assert
   [[:var :y :public [:int 0 10]]
    [:var :z :public [:int -5 0]]
    [:var :|z| :proto [:int 0 5]]
    [:constraint [:abs [:|z| := :z]]]
    [:constraint ['arithm [:y '= :|z|]]]]

   [($in :y 0 10)
    ($in :z -5 0)
    ($= :y ($abs :z))])

  (compiled-assert
   [[:var :x :public [:int -5 5]]
    [:var :|x| :proto [:int 0 5]]
    [:constraint [:abs [:|x| := :x]]]
    [:constraint ['arithm [:|x| '= 2]]]]
   [($in :x -5 5)
    ($= ($abs :x) 2)])
  )
