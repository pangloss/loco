(ns loco.model.resource
  (:require [loco.model :as model])
  (:use clojure.test
        loco.core
        loco.constraints))

(deftest $knapsack-test
  (is
   (=
    [[:var :W :public [:int 0 200]]
     [:var :V :public [:int 0 200]]
     [:constraint
      [:knapsack
       [[:weights [3 1 2]]
        [:values [5 6 7]]
        [:occurrences [:x :y :z]]
        [:total-weight :W]
        [:total-value :V]]]]]
    (->> [($in :W 0 200)
          ($in :V 0 200)
          ($knapsack [3 1 2]                ; weights
                     [5 6 7]                ; values
                     [:x :y :z]             ; occurrences
                     :W                     ; total weight
                     :V)]                   ; total value
         model/compile)))

  )
