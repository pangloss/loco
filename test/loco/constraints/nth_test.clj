;; FIXME: finish $nth
#_(ns loco.constraints.nth-test
  (:require
   [clojure.test :refer :all]
   [loco.constraints :refer :all]
   [loco.constraints.test-utils :refer :all]
   ))

#_(deftest nth-test
  (testing
      (is
       (loco?
        [($in :a 100 200)
         ($in :index 0 5)
         ($= 4 ($nth [:a 2 3 4 5] :index))]
        {
         :identity []
         :model []
         :compiled []
         :solutions
         #{{:int-var 4, :b1 0, :b2 0, :b3 1, :b4 0}
           {:int-var 8, :b1 0, :b2 0, :b3 0, :b4 1}
           {:int-var 0, :b1 0, :b2 0, :b3 0, :b4 0}
           {:int-var 2, :b1 0, :b2 1, :b3 0, :b4 0}}}
        )))

  #_(compiled-assert
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
     ['arithm
      [:4 '= :$nth_:a_:2_:3_:4_:5_:at_:index_:offset_0]]]]



   "when $nth has a mix of consts and vars, should make IntVars of consts")

  #_(compiled-assert
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
     ['arithm [:4 '= :$nth_1_2_3_5_8_13_:at_:index_:offset_0]]]]

   [($in :index 0 5)
    ($= 4 ($nth [1 2 3 5 8 13] :index))])

  #_(compiled-assert
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
     ['arithm [:4 '= :$nth_1_2_3_5_8_13_:at_:index_:offset_2]]]]

   [($in :index 0 2)
    ($= 4 ($nth [1 2 3 5 8 13] :index 2))])

  #_(compiled-assert
   [[:var :index :public [:int 0 2]]
    [:var :4 :hidden [:const 4]]
    [:var :$nth_473901430 :proto [:int 3 5]]
    [:constraint
     [:element [:$nth_473901430
                [:in [1 2 3 4 5 3 4 5 6 7 8 9]]
                [:at :index]
                [:offset 2]]]]
    [:constraint ['arithm [:4 '= :$nth_473901430]]]]

   [($in :index 0 2)
    ($= 4 ($nth [1 2 3 4 5 3 4 5 6 7 8 9] :index 2))])

  #_(compiled-assert
   [[:var :index :public [:int 0 2]]
    [:var :array-val :public [:int 1 5]]
    [:var :$nth_1_2_3_4_5_:at_:index_:offset_0 :proto [:int 1 3]]
    [:constraint [:element
                  [:$nth_1_2_3_4_5_:at_:index_:offset_0
                   [:in [1 2 3 4 5]]
                   [:at :index]
                   [:offset 0]]]]
    [:constraint
     ['arithm [:array-val '= :$nth_1_2_3_4_5_:at_:index_:offset_0]]]]

   [($in :index 0 2)
    ($in :array-val 1 5)
    ($= :array-val ($nth [1 2 3 4 5] :index))])
  )
