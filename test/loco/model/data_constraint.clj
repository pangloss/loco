(ns ^:model loco.model.data-constraint
  (:use clojure.test
        loco.model.test
        loco.constraints))

(deftest $element-test
  (compiled-assert
   [[:var :index :public [:int 0 2]]
    [:var :array-val :public [:int 1 5]]
    [:constraint
     [:element
      [:array-val [:in [1 2 3 4 5]] [:at :index] [:offset 0]]]]]

   [($in :index 0 2)
    ($in :array-val 1 5)
    ($element :array-val [1 2 3 4 5] :index)])

  (compiled-assert
   [[:var :a :public [:int 10 99]]
    [:var :b :public [:int 0 9]]
    [:var :c :public [:int 100 1000]]
    [:var :index :public [:int 0 2]]
    [:var :array-val :public [:int 100 1000]]
    [:constraint [:element [:array-val [:in [:a :b :c]] [:at :index] [:offset 2]]]]]

   [($in :a 10 99)
    ($in :b 0 9)
    ($in :c 100 1000)
    ($in :index 0 2)
    ($in :array-val 100 1000)
    ($element :array-val [:a :b :c] :index 2)])

  )

(deftest $nth-test
  (compiled-assert
   [[:var :a :public [:int 100 200]]
    [:var :index :public [:int 0 5]]
    [:var :4 :hidden [:const 4]]
    [:var :2 :hidden [:const 2]]
    [:var :3 :hidden [:const 3]]
    [:var :5 :hidden [:const 5]]
    [:var :$nth_:a_:2_:3_:4_:5_:at_:index_:offset_0 :proto [:int 2 200]]
    [:constraint
     [:element
      [:$nth_:a_:2_:3_:4_:5_:at_:index_:offset_0
       [:in [:a :2 :3 :4 :5]]
       [:at :index]
       [:offset 0]]]]
    [:constraint
     [:arithm
      [:4 := :$nth_:a_:2_:3_:4_:5_:at_:index_:offset_0]]]]

   [($in :a 100 200)
    ($in :index 0 5)
    ($= 4 ($nth [:a 2 3 4 5] :index))]

   "when $nth has a mix of consts and vars, should make IntVars of consts")

  (compiled-assert
   [[:var :index :public [:int 0 5]]
    [:var :4 :hidden [:const 4]]
    [:var :$nth_1_2_3_5_8_13_:at_:index_:offset_0 :proto [:int 1 13]]
    [:constraint
     [:element
      [:$nth_1_2_3_5_8_13_:at_:index_:offset_0
       [:in [1 2 3 5 8 13]]
       [:at :index]
       [:offset 0]]]]
    [:constraint
     [:arithm [:4 := :$nth_1_2_3_5_8_13_:at_:index_:offset_0]]]]

   [($in :index 0 5)
    ($= 4 ($nth [1 2 3 5 8 13] :index))])

  (compiled-assert
   [[:var :index :public [:int 0 2]]
    [:var :4 :hidden [:const 4]]
    [:var :$nth_1_2_3_5_8_13_:at_:index_:offset_2 :proto [:int 3 8]]
    [:constraint
     [:element
      [:$nth_1_2_3_5_8_13_:at_:index_:offset_2
       [:in [1 2 3 5 8 13]]
       [:at :index]
       [:offset 2]]]]
    [:constraint
     [:arithm [:4 := :$nth_1_2_3_5_8_13_:at_:index_:offset_2]]]]

   [($in :index 0 2)
    ($= 4 ($nth [1 2 3 5 8 13] :index 2))])

  (compiled-assert
   [[:var :index :public [:int 0 2]]
    [:var :4 :hidden [:const 4]]
    [:var :$nth_473901430 :proto [:int 3 5]]
    [:constraint
     [:element [:$nth_473901430
                [:in [1 2 3 4 5 3 4 5 6 7 8 9]]
                [:at :index]
                [:offset 2]]]]
    [:constraint [:arithm [:4 := :$nth_473901430]]]]

   [($in :index 0 2)
    ($= 4 ($nth [1 2 3 4 5 3 4 5 6 7 8 9] :index 2))])

  (compiled-assert
   [[:var :index :public [:int 0 2]]
    [:var :array-val :public [:int 1 5]]
    [:var :$nth_1_2_3_4_5_:at_:index_:offset_0 :proto [:int 1 3]]
    [:constraint [:element
                  [:$nth_1_2_3_4_5_:at_:index_:offset_0
                   [:in [1 2 3 4 5]]
                   [:at :index]
                   [:offset 0]]]]
    [:constraint
     [:arithm [:array-val := :$nth_1_2_3_4_5_:at_:index_:offset_0]]]]

   [($in :index 0 2)
    ($in :array-val 1 5)
    ($= :array-val ($nth [1 2 3 4 5] :index))])
  )

(deftest bit-channeling-test
  (compiled-assert
   [[:var :int-var :public [:int 0 16]]
    [:var :b1 :public [:bool 0 1]]
    [:var :b2 :public [:bool 0 1]]
    [:var :b3 :public [:bool 0 1]]
    [:var :b4 :public [:bool 0 1]]
    [:constraint [:bit-channeling
                  [:bool-vars [:b1 :b2 :b3 :b4]]
                  [:int-var :int-var]]]]

   [($in :int-var 0 16)
    ($bits-channeling [:b1 :b2 :b3 :b4] :int-var)]
   "should generate bool vars")

  (compiled-assert
   [[:var :int-var :public [:int 0 16]]
    [:var :b1 :hidden [:bool 0 1]]
    [:var :b2 :public [:bool 0 1]]
    [:var :b3 :public [:bool 0 1]]
    [:var :b4 :public [:bool 0 1]]
    [:constraint [:bit-channeling
                  [:bool-vars [:b1 :b2 :b3 :b4]]
                  [:int-var :int-var]]]]

   [($in :int-var 0 16)
    ($bool- :b1)
    ($bits-channeling [:b1 :b2 :b3 :b4] :int-var)]
   "should remove overlapping generated vars")

  )
