(ns loco.model.data-constraint
  (:require [loco.model :as model])
  (:use clojure.test
        loco.core
        loco.constraints))

(deftest $element-test
  (is
   (=
    [[:var :index :public [:int 0 2]]
     [:var :array-val :public [:int 1 5]]
     [:constraint [:element [:array-val :in [1 2 3 4 5] :at :index]]]]
    (->>
     [($in :index 0 2)
      ($in :array-val 1 5)
      ($element :array-val [1 2 3 4 5] :index)]
     model/compile)))

  (is
   (=
    [[:var :a :public [:int 10 99]]
     [:var :b :public [:int 0 9]]
     [:var :b :public [:int 100 1000]]
     [:var :index :public [:int 0 2]]
     [:var :array-val :public [:int 100 1000]]
     [:constraint [:element [:array-val :in [:a :b :c] :at :index :offset 2]]]]
     (->>
      [($in :a 10 99)
       ($in :b 0 9)
       ($in :b 100 1000)
       ($in :index 0 2)
       ($in :array-val 100 1000)
       ($element :array-val [:a :b :c] :index 2)]
      model/compile))))

(deftest $nth-test
  (is
   (=
    [[:var :a :public [:int 100 200]]
     [:var :index :public [:int 0 5]]
     [:var :4 :hidden [:const 4]]
     [:var :2 :hidden [:const 2]]
     [:var :3 :hidden [:const 3]]
     [:var :4 :hidden [:const 4]]
     [:var :5 :hidden [:const 5]]
     [:var :element_:a_:2_:3_:4_:5_:at_:index :proto [:int 2 200]]
     [:constraint [:element [:element_:a_:2_:3_:4_:5_:at_:index :in [:a :2 :3 :4 :5] :at :index]]]
     [:constraint [:all-equal [:4 :element_:a_:2_:3_:4_:5_:at_:index]]]]
    (->>
     [($in :a 100 200)
      ($in :index 0 5)
      ($= 4 ($nth [:a 2 3 4 5] :index))]
     model/compile))
   "when $nth has a mix of consts and vars, should make IntVars of consts")

  (is
   (=
    [[:var :index :public [:int 0 5]]
     [:var :4 :hidden [:const 4]]
     [:var :element_1_2_3_5_8_13_:at_:index :proto [:int 1 13]]
     [:constraint [:element [:element_1_2_3_5_8_13_:at_:index :in [1 2 3 5 8 13] :at :index]]]
     [:constraint [:all-equal [:4 :element_1_2_3_5_8_13_:at_:index]]]]
    (->>
     [($in :index 0 5)
      ($= 4 ($nth [1 2 3 5 8 13] :index))]
     model/compile)))

  (is
   (=
    [[:var :index :public [:int 0 2]]
     [:var :4 :hidden [:const 4]]
     [:var :element_1_2_3_5_8_13_:at_:index_:offset_2 :proto [:int 3 8]]
     [:constraint
      [:element
       [:element_1_2_3_5_8_13_:at_:index_:offset_2 :in [1 2 3 5 8 13] :at :index :offset [2]]]]
     [:constraint [:all-equal [:4 :element_1_2_3_5_8_13_:at_:index_:offset_2]]]]
    (->>
     [($in :index 0 2)
      ($= 4 ($nth [1 2 3 5 8 13] :index 2))]
     model/compile)))

  (is
   (=
    [[:var :index :public [:int 0 2]]
     [:var :4 :hidden [:const 4]]
     [:var :element_1132179721 :proto [:int 3 5]]
     [:constraint
      [:element [:element_1132179721 :in [1 2 3 4 5 3 4 5 6 7 8 9] :at :index :offset [2]]]]
     [:constraint [:all-equal [:4 :element_1132179721]]]]
    (->>
     [($in :index 0 2)
      ($= 4 ($nth [1 2 3 4 5 3 4 5 6 7 8 9] :index 2))]
     model/compile)))

  (->>
   [($in :index 0 2)
    ($in :array-val 1 5)
    ($= :array-val ($nth [1 2 3 4 5] :index))]
   model/compile))
