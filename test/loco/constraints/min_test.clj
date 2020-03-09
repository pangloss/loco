(ns loco.constraints.min-test
  (:require
   [clojure.test :refer :all]
   [loco.constraints :refer :all]
   [loco.constraints.test-utils :refer :all]
   ))

(deftest min-test
  (testing "partial min form"
    (test-loco
     [($in :y 0 1)
      ($in :z 1 2)
      ($in :x 2 3)
      ($=  :z ($min [:x :y]))]
     {:model '[[:var :y :public [:bool 0 1]]
               [:var :z :public [:int 1 2]]
               [:var :x :public [:int 2 3]]
               [:var "min_x_y" :proto [:int 0 1]]
               [min ["min_x_y" [of [:x :y]]]]
               [arithm [:z = "min_x_y"]]],
      :compiled [["y = [0,1]" "z = {1..2}" "x = {2..3}" "min_x_y = [0,1]"]
                 ["MIN ([min_x_y = [0,1].MIN(x = {2..3},y = [0,1])])"
                  "ARITHM ([prop(z.EQ.min_x_y)])"]],
      :solutions #{{:y 1, :z 1, :x 2}
                   {:y 1, :z 1, :x 3}}}
     )
    )

  (testing "relational"
    (testing "ints"
      (test-loco
       [($in :y 0 1)
        ($in :z 1 2)
        ($in :x 2 3)
        ($min :z [:x :y])]
       {:model
        '[[:var :y :public [:bool 0 1]]
          [:var :z :public [:int 1 2]]
          [:var :x :public [:int 2 3]]
          [min [:z [of [:x :y]]]]],
        :compiled
        [["y = [0,1]" "z = {1..2}" "x = {2..3}"]
         ["MIN ([z = {1..2}.MIN(x = {2..3},y = [0,1])])"]],
        :solutions #{{:y 1, :z 1, :x 2}
                     {:y 1, :z 1, :x 3}}}
       ))

    (testing "bools"
      (test-loco
       [($bool :y)
        ($bool :z)
        ($bool :x)
        ($min :z [:x :y])]
       {:model '[[:var :y :public [:bool 0 1]]
                 [:var :z :public [:bool 0 1]]
                 [:var :x :public [:bool 0 1]]
                 [min [:z [of [:x :y]]]]],
        :compiled [["y = [0,1]" "z = [0,1]" "x = [0,1]"]
                   ["MIN ([z = [0,1].MIN(x = [0,1],y = [0,1])])"]],
        :solutions #{{:y 1, :z 1, :x 1} {:y 0, :z 0, :x 0} {:y 0, :z 0, :x 1}
                     {:y 1, :z 0, :x 0}}}
       )
      )

    (testing "set"
      (testing "with empty sets allowed"
        (test-loco
         [
          ($set :set #{} #{0 1 2 3})
          ($int :min [0 1])
          ($min :set :min false)]
         {:model '[[:var :set :public [:set #{} #{0 1 2 3}]]
                   [:var :min :public [:bool 0 1]]
                   [min [:min [of :set] [not-empty? false]]]],
          :compiled [["set = [{}, {0, 1, 2, 3}]" "min = [0,1]"]
                     ["SETMIN ([PropMinElement(set, min)])"]],
          :solutions #{
                       {:set #{1 3},   :min 1} {:set #{1},       :min 1} {:set #{},      :min 1}
                       {:set #{0 1},   :min 0} {:set #{0},       :min 0} {:set #{0 1 3}, :min 0}
                       {:set #{1 2},   :min 1} {:set #{0 3 2},   :min 0}
                       {:set #{0 1 2}, :min 0} {:set #{1 3 2},   :min 1}
                       {:set #{0 3},   :min 0} {:set #{0 1 3 2}, :min 0}
                       {:set #{0 2},   :min 0} {:set #{},        :min 0}
                       }}
         ))

      (testing "without empty sets"
        (test-loco
         [
          ($set :set #{} #{0 1 2 3})
          ($int :min [0 1])
          ($min :set :min true)]
         {:compiled [["set = [{}, {0, 1, 2, 3}]" "min = [0,1]"]
                     ["SETMIN ([PropNotEmpty(set), PropMinElement(set, min)])"]],
          :solutions #{
                       {:set #{1 3},     :min 1} {:set #{1},     :min 1} {:set #{0 1}, :min 0}
                       {:set #{0},       :min 0} {:set #{0 1 3}, :min 0} {:set #{1 2}, :min 1}
                       {:set #{0 3 2},   :min 0} {:set #{0 1 2}, :min 0}
                       {:set #{1 3 2},   :min 1} {:set #{0 3},   :min 0}
                       {:set #{0 1 3 2}, :min 0} {:set #{0 2},   :min 0}
                       }}
         ))

      (testing "indices"
        (testing "with empty sets allowed"
          (test-loco
           [
            ($set :indicies #{} #{0 1 2 3 4})
            ($int :min 0 5)
            ($min :indicies [1 2 3 5 8] 0 :min false)]
           {:model '[[:var :indicies :public [:set #{} #{0 1 2 3 4}]]
                     [:var :min :public [:int 0 5]]
                     [min [:min [of [1 2 3 5 8]] [indices :indicies] [offset 0] [not-empty? false]]]],
            :compiled [["indicies = [{}, {0, 1, 2, 3, 4}]" "min = {0..5}"]
                       ["SETMIN ([PropMinElement(indicies, min)])"]],
            :solutions #{
                         {:indicies #{0 4 2},     :min 1} {:indicies #{1 3},     :min 2}
                         {:indicies #{4 3 2},     :min 3} {:indicies #{0 4 3 2}, :min 1}
                         {:indicies #{4 2},       :min 3} {:indicies #{2},       :min 3}
                         {:indicies #{3},         :min 5} {:indicies #{3 2},     :min 3}
                         {:indicies #{},          :min 2} {:indicies #{},        :min 5}
                         {:indicies #{0 1},       :min 1} {:indicies #{},        :min 3}
                         {:indicies #{0},         :min 1} {:indicies #{1 4 2},   :min 2}
                         {:indicies #{1 4 3 2},   :min 2} {:indicies #{0 4 3},   :min 1}
                         {:indicies #{},          :min 0} {:indicies #{1 4},     :min 2}
                         {:indicies #{0 1 4 3},   :min 1} {:indicies #{1},       :min 2}
                         {:indicies #{1 3 2},     :min 2} {:indicies #{0 3},     :min 1}
                         {:indicies #{1 4 3},     :min 2} {:indicies #{1 2},     :min 2}
                         {:indicies #{0 1 2},     :min 1} {:indicies #{},        :min 1}
                         {:indicies #{0 1 4 3 2}, :min 1} {:indicies #{4 3},     :min 5}
                         {:indicies #{0 4},       :min 1} {:indicies #{0 1 3 2}, :min 1}
                         {:indicies #{0 2},       :min 1} {:indicies #{0 3 2},   :min 1}
                         {:indicies #{0 1 4 2},   :min 1} {:indicies #{0 1 4},   :min 1}
                         {:indicies #{},          :min 4} {:indicies #{0 1 3},   :min 1}
                         }}
           ))

        (testing "with empty sets"
          (test-loco
           [
            ($set :indicies #{} #{0 1 2 3 4})
            ($int :min 0 5)
            ($min :indicies [1 2 3 5 8] 0 :min true)]
           {:model '[[:var :indicies :public [:set #{} #{0 1 2 3 4}]]
                     [:var :min :public [:int 0 5]]
                     [min [:min [of [1 2 3 5 8]] [indices :indicies] [offset 0] [not-empty? true]]]],
            :compiled [["indicies = [{}, {0, 1, 2, 3, 4}]" "min = {0..5}"]
                       ["SETMIN ([PropNotEmpty(indicies), PropMinElement(indicies, min)])"]],
            :solutions #{
                         {:indicies #{0 4 2},   :min 1} {:indicies #{1 3},       :min 2}
                         {:indicies #{4 3 2},   :min 3} {:indicies #{0 4 3 2},   :min 1}
                         {:indicies #{4 2},     :min 3} {:indicies #{2},         :min 3}
                         {:indicies #{3},       :min 5} {:indicies #{3 2},       :min 3}
                         {:indicies #{0 1},     :min 1} {:indicies #{0},         :min 1}
                         {:indicies #{1 4 2},   :min 2} {:indicies #{1 4 3 2},   :min 2}
                         {:indicies #{0 4 3},   :min 1} {:indicies #{1 4},       :min 2}
                         {:indicies #{0 1 4 3}, :min 1} {:indicies #{1},         :min 2}
                         {:indicies #{1 3 2},   :min 2} {:indicies #{0 3},       :min 1}
                         {:indicies #{1 4 3},   :min 2} {:indicies #{1 2},       :min 2}
                         {:indicies #{0 1 2},   :min 1} {:indicies #{0 1 4 3 2}, :min 1}
                         {:indicies #{4 3},     :min 5} {:indicies #{0 4},       :min 1}
                         {:indicies #{0 1 3 2}, :min 1} {:indicies #{0 2},       :min 1}
                         {:indicies #{0 3 2},   :min 1} {:indicies #{0 1 4 2},   :min 1}
                         {:indicies #{0 1 4},   :min 1} {:indicies #{0 1 3},     :min 1}
                         }}
           ))))))
