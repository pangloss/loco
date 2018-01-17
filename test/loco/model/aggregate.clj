(ns ^:model loco.model.aggregate
  (:use clojure.test
        loco.model.test
        loco.constraints))

;; http://sofdem.github.io/gccat/gccat/Kaggregate.html#uid3018
;; summation is an aggregate, but it's in the arithmetic tests

(deftest min-test
  (compiled-assert
   [[:var :y :public [:int 0 10]]
    [:var :z :public [:int 0 10]]
    [:var :x :public [:int 0 10]]
    [:var :a :public [:int 0 10]]
    [:constraint [:min [:z [:of [:x :y :a]]]]]]

   [($in :y 0 10)
    ($in :z 0 10)
    ($in :x 0 10)
    ($in :a 0 10)
    ($min :z [:x :y :a])])

  (compiled-assert
   [[:var :y :public [:int 0 10]]
    [:var :z :public [:int 0 1]]
    [:var :x :public [:int 0 5]]
    [:var :min_x_y :proto [:int 0 5]]
    [:constraint [:min [:min_x_y [:of [:x :y]]]]]
    [:constraint [:arithm [:z := :min_x_y]]]]

   [($in :y 0 10)
    ($in :z 0 1)
    ($in :x 0 5)
    ($= :z ($min [:x :y]))])

  (compiled-assert
   [[:constraint
     [:min
      [:min
       [:of [9 3 28 1 4 50 6 2 100]]
       [:indices :set-indices]
       [:offset 4]
       [:not-empty? true]]]]]
   [($min :set-indices [9 3 28 1 4 50 6 2 100] 4 :min true)])

  (compiled-assert
   [[:constraint
     [:min [:min [:of :set-indices] [:not-empty? true]]]]]
   [($min :set-indices :min true)])

  (is (=
       [[:constraint :partial [:min [1 2]]]
        [:constraint :partial [:min [1 2]]]
        [:constraint :partial [:min [1]]]
        [:constraint :partial [:min [1 2 3]]]
        [:constraint :partial [:min [1 2 3 4 5]]]
        [:constraint :partial [:min [1 2 3 4 5 6]]]]
       [($min [1 2])
        ($min 1 2)
        ($min 1)
        ($min 1 2 3)
        ($min 1 2 3 4 5)
        ($min 1 2 3 4 5 6)]))
  )

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
    [:constraint [:arithm [:z := :max_x_y]]]]

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

(deftest scalar-test
  (compiled-assert
   [
    [:var :1s :public [:int 0 9]]
    [:var :10s :public [:int 0 9]]
    [:var :100s :public [:int 0 9]]
    [:var :999? :public [:int 0 999]]
    [:constraint [:scalar [:999? := [:1s :10s :100s] [1 10 100]]]]]

   [($in :1s 0 9)
    ($in :10s 0 9)
    ($in :100s 0 9)
    ($in :999? 0 999)
    ($scalar :999? := [:1s :10s :100s] [1 10 100])])

  (compiled-assert
   [[:var :1s :public [:int -1 9]]
    [:var :10s :public [:int 0 9]]
    [:var :100s :public [:int -1 9]]
    [:var :555 :hidden [:const 555]]
    [:var :scalar_1922668695 :proto [:int -136 1026]]
    [:constraint [:scalar [:scalar_1922668695 := [:1s :10s :100s :1s :1s :1s] [1 10 100 1 1 1]]]]
    [:constraint [:arithm [:555 := :scalar_1922668695]]]]

   [($in :1s -1 9)
    ($in :10s 0 9)
    ($in :100s -1 9)
    ($= 555 ($scalar [:1s :10s :100s :1s :1s :1s] [1 10 100 1 1 1]))]
   )

  (compiled-assert
   [[:var :1s :public [:int -1 9]]
    [:var :10s :public [:int 0 9]]
    [:var :100s :public [:int -1 9]]
    [:var :555 :hidden [:const 555]]
    [:var :scalar_1s*1+10s*10+100s*100 :proto [:int -109 999]]
    [:constraint [:scalar [:scalar_1s*1+10s*10+100s*100 := [:1s :10s :100s] [1 10 100]]]]
    [:constraint [:arithm [:555 := :scalar_1s*1+10s*10+100s*100]]]]

   [($in :1s -1 9)
    ($in :10s 0 9)
    ($in :100s -1 9)
    ($= 555 ($scalar [:1s :10s :100s] [1 10 100]))])

  (compiled-assert
   [[:var :555 :hidden [:const 555]]
    [:var :1 :hidden [:const 1]]
    [:var :2 :hidden [:const 2]]
    [:var :3 :hidden [:const 3]]
    [:var :scalar_1*1+2*10+3*100 :proto [:int 14 321]]
    [:constraint
     [:scalar [:scalar_1*1+2*10+3*100 := [:1 :2 :3] [1 10 100]]]]
    [:constraint [:arithm [:555 := :scalar_1*1+2*10+3*100]]]]

   [($= 555 ($scalar [1 2 3] [1 10 100]))])

  )
