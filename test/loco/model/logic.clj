(ns ^:model loco.model.logic
  (:use clojure.test
        loco.model.test
        loco.constraints))

(deftest logic-test
  (compiled-assert
   [[:var :x :public [:int 0 0]]
    [:constraint :true]
    [:constraint [:not [:constraint :false]]]
    [:constraint [:not [:constraint [:not [:constraint :true]]]]]
    [:constraint [:and [[:constraint :true] [:constraint :true]]]]
    [:constraint
     [:not
      [:constraint [:and [[:constraint :true] [:constraint :false]]]]]]
    [:constraint [:or [[:constraint :true] [:constraint :false]]]]
    [:constraint
     [:if-else
      [[:constraint :true] [:constraint :true] [:constraint :false]]]]
    [:constraint [:when [[:constraint :false] [:constraint :false]]]]
    [:constraint
     [:if-else
      [[:constraint :false] [:constraint :false] [:constraint :true]]]]]

   [($in :x 0 0)
    ($true)
    ($not ($false))
    ($not ($not ($true)))
    ($and ($true) ($true))
    ($not ($and ($true) ($false)))
    ($or ($true) ($false))
    ($if ($true) ($true) ($false))
    ($when ($false) ($false))
    ($if ($false) ($false) ($true))
    ])

  (binding [loco.constraints.logic.logic/*cond-name-gen* (fn [_] "")]
    (compiled-assert
     [[:var :_if_cond_0 :hidden [:bool 0 1]]
      [:var :_if_0 :hidden [:bool 0 1]]
      [:var :_not_if_0 :hidden [:bool 0 1]]
      [:var :_if_cond_1 :hidden [:bool 0 1]]
      [:var :_if_1 :hidden [:bool 0 1]]
      [:var :_not_if_1 :hidden [:bool 0 1]]
      [:var :_if_cond_2 :hidden [:bool 0 1]]
      [:var :_if_2 :hidden [:bool 0 1]]
      [:var :_not_if_2 :hidden [:bool 0 1]]
      [:var :_if_cond_3 :hidden [:bool 0 1]]
      [:var :_if_3 :hidden [:bool 0 1]]
      [:var :_not_if_3 :hidden [:bool 0 1]]
      [:reify :_if_cond_0 [:constraint :false]]
      [:reify :_if_0 [:constraint [:and [:_if_cond_0]]]]
      [:reify :_not_if_0 [:constraint [:not [:constraint :false]]]]
      [:reify :_if_cond_1 [:constraint :false]]
      [:reify :_if_1 [:constraint [:and [:_if_cond_1 :_not_if_0]]]]
      [:reify :_not_if_1 [:constraint [:not [:constraint :false]]]]
      [:reify :_if_cond_2 [:constraint :true]]
      [:reify :_if_2 [:constraint [:and [:_if_cond_2 :_not_if_0 :_not_if_1]]]]
      [:reify :_not_if_2 [:constraint [:not [:constraint :true]]]]
      [:reify :_if_cond_3 [:constraint :false]]
      [:reify :_if_3 [:constraint [:and [:_if_cond_3 :_not_if_0 :_not_if_1 :_not_if_2]]]]
      [:reify :_not_if_3 [:constraint [:not [:constraint :false]]]]
      [:constraint [:if-else [:_if_0 [:constraint :true] [:constraint [:and [:_not_if_0]]]]]]
      [:constraint [:if-else [:_if_1 [:constraint :false] [:constraint [:and [:_not_if_1]]]]]]
      [:constraint [:if-else [:_if_2 [:constraint :true] [:constraint [:and [:_not_if_2]]]]]]
      [:constraint [:if-else [:_if_3 [:constraint :true] [:constraint [:and [:_not_if_3]]]]]]
      [:constraint [:when [[:constraint [:and [:_not_if_0 :_not_if_1 :_not_if_2 :_not_if_3]]]
                           [:constraint :true]]]]]
     ($cond
      ($false) ($true)
      ($false) ($false)
      ($true) ($true)
      ($false) ($true)
      :else ($true))
     )
    )
  )

(deftest reify-test
  (compiled-assert
   [[:var :a :hidden [:bool 0 1]]
    [:reify :a [:constraint [:and [[:constraint :true]]]]]]
   [($reify :a ($and ($true)))])

  (compiled-assert
   [[:var :b :public [:int 0 2]]
    [:var :c :public [:int 0 2]]
    [:var :a :hidden [:bool 0 1]]
    [:var :2 :hidden [:const 2]]
    [:var :b+2 :proto [:int 2 4]]
    [:constraint ['sum [:b+2 '= [:b :2]]]]
    [:reify :a [:constraint ['arithm [:c '= :b+2]]]]]
   [($in :b 0 2)
    ($in :c 0 2)
    ($reify :a ($= :c ($+ :b 2)))])

  (compiled-assert
   [[:var :a :public [:bool 0 1]]
    [:reify :a [:constraint [:and [[:constraint :true]]]]]]
   [($bool :a)
    ($reify :a ($and ($true)))]
   "should preserve user declared var")
  )
