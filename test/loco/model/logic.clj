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
      [[:constraint :false] [:constraint :false] [:constraint :true]]]]
    [:constraint
     [:if-else
      [[:constraint :false]
       [:constraint :true]
       [:constraint
        [:if-else
         [[:constraint :false]
          [:constraint :false]
          [:constraint
           [:if-else
            [[:constraint :true]
             [:constraint :true]
             [:constraint
              [:if-else
               [[:constraint :false]
                [:constraint :true]
                [:constraint :true]]]]]]]]]]]]]]

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
    ($cond
     ($false) ($true)
     ($false) ($false)
     ($true) ($true)
     ($false) ($true)
     :else ($true))])

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
    [:constraint [:sum [:b+2 := [:b :2]]]]
    [:reify :a [:constraint [:arithm [:c := :b+2]]]]]
   [($in :b 0 2)
    ($in :c 0 2)
    ($reify :a ($= :c ($+ :b 2)))])

  (compiled-assert
   [[:var :a :public [:bool 0 1]]
    [:reify :a [:constraint [:and [[:constraint :true]]]]]]
   [($bool :a)
    ($reify :a ($and ($true)))]
   "should preserve user declared var")

  ;;TODO: implement reify-partial...
  [($in :x 0 1)
   ($= ($reify ($true)) :x)
   ($= ($reify ($false)) ($- 1 :x))]

  )
