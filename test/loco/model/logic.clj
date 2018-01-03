(ns loco.model.logic
  (:require [loco.model :as model])
  (:use clojure.test
        loco.core
        loco.constraints))

(deftest logic-test
  (is
   (=
    [[:var :x :public [:int [1]]]
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
    [($in :x [1])
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
      :else ($true))]))

  )

(deftest reify-test
  [($in :x 0 1)
   ($= ($reify ($true)) :x)
   ($= ($reify ($false)) ($- 1 :x))]

  )
