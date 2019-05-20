(ns ^:model loco.model.aggregate
  (:use clojure.test
        loco.model.test
        loco.constraints))

;; http://sofdem.github.io/gccat/gccat/Kaggregate.html#uid3018
;; summation is an aggregate, but it's in the arithmetic tests

(deftest max-test
  (compiled-assert
   [[:var :y :public [:int 0 10]]
    [:var :z :public [:int 0 10]]
    [:var :x :public [:int 0 10]]
    [:var :a :public [:int 0 10]]
    [:constraint [:max [:z [:of [:x :y :a]]]]]]

   [($in :y 0 10)
    ($in :z 0 10)
    ($in :x 0 10)
    ($in :a 0 10)
    ($max :z [:x :y :a])])

  (compiled-assert
   [[:var :y :public [:int 0 10]]
    [:var :z :public [:int 0 1]]
    [:var :x :public [:int 0 5]]
    [:var :max_x_y :proto [:int 0 10]]
    [:constraint [:max [:max_x_y [:of [:x :y]]]]]
    [:constraint ['arithm [:z '= :max_x_y]]]]

   [($in :y 0 10)
    ($in :z 0 1)
    ($in :x 0 5)
    ($= :z ($max [:x :y]))])

  (compiled-assert
   [[:constraint
     [:max
      [:max
       [:of [9 3 28 1 4 50 6 2 100]]
       [:indices :set-indices]
       [:offset 4]
       [:not-empty? true]]]]]
   [($max :set-indices [9 3 28 1 4 50 6 2 100] 4 :max true)])

  (compiled-assert
   [[:constraint
     [:max [:max [:of :set-indices] [:not-empty? true]]]]]
   [($max :set-indices :max true)])

  (is (=
       [[:constraint :partial [:max [1 2]]]
        [:constraint :partial [:max [1 2]]]
        [:constraint :partial [:max [1]]]
        [:constraint :partial [:max [1 2 3]]]
        [:constraint :partial [:max [1 2 3 4 5]]]
        [:constraint :partial [:max [1 2 3 4 5 6]]]]
       [($max [1 2])
        ($max 1 2)
        ($max 1)
        ($max 1 2 3)
        ($max 1 2 3 4 5)
        ($max 1 2 3 4 5 6)]))
  )
