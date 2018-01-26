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
