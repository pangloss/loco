(ns ^:model loco.model.resource
  (:use clojure.test
        loco.model.test
        loco.constraints))

(deftest $knapsack-test
  (compiled-assert
   [[:var :W :public [:int 0 200]]
    [:var :V :public [:int 0 200]]
    [:constraint
     [:knapsack
      [[:weight [3 1 2]]
       [:energy [5 6 7]]
       [:occurrences [:x :y :z]]
       [:weight-sum :W]
       [:energy-sum :V]]]]]

   [($in :W 0 200)
    ($in :V 0 200)
    ($knapsack [3 1 2]                ; weights
               [5 6 7]                ; values
               [:x :y :z]             ; occurrences
               :W                     ; total weight
               :V)]                   ; total value
   )

  )
