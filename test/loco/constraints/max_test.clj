(ns loco.constraints.max-test
  (:require
   [clojure.test :refer :all]
   [loco.constraints :refer :all]
   [loco.constraints.test-utils :refer :all]
   ))

(deftest max-test
  (testing "partial max form"
    (test-loco
     [($in :y 0 1)
      ($in :z 1 2)
      ($in :x 2 3)
      ($= :z ($max [:x :y]))]
     {:model '[[:var :y :public [:bool 0 1]]
               [:var :z :public [:int 1 2]]
               [:var :x :public [:int 2 3]]
               [:var "max_x_y" :proto [:int 2 3]]
               [max ["max_x_y" [of [:x :y]]]]
               [arithm [:z = "max_x_y"]]],
      :compiled [["y = [0,1]" "z = {1..2}" "x = {2..3}" "max_x_y = {2..3}"]
                 ["MAX ([max_x_y = {2..3}.MAX(x = {2..3},y = [0,1])])"
                  "ARITHM ([prop(z.EQ.max_x_y)])"]],
      :solutions #{{:y 1, :z 2, :x 2}
                   {:y 0, :z 2, :x 2}}}
     )
    )

  (testing "relational"
    (testing "ints"
      (test-loco
       [($in :y 0 1)
        ($in :z 1 2)
        ($in :x 2 3)
        ($max :z [:x :y])]
       {:model '[[:var :y :public [:bool 0 1]]
                 [:var :z :public [:int 1 2]]
                 [:var :x :public [:int 2 3]]
                 [max [:z [of [:x :y]]]]],
        :compiled [["y = [0,1]" "z = {1..2}" "x = {2..3}"]
                   ["MAX ([z = {1..2}.MAX(x = {2..3},y = [0,1])])"]],
        :solutions #{{:y 1, :z 2, :x 2}
                     {:y 0, :z 2, :x 2}}}
       ))

    (testing "bools"
      (test-loco
       [($bool :y)
        ($bool :z)
        ($bool :x)
        ($max :z [:x :y])]
       {:model '[[:var :y :public [:bool 0 1]]
                 [:var :z :public [:bool 0 1]]
                 [:var :x :public [:bool 0 1]]
                 [max [:z [of [:x :y]]]]],
        :compiled [["y = [0,1]" "z = [0,1]" "x = [0,1]"]
                   ["MAX ([z = [0,1].MAX(x = [0,1],y = [0,1])])"]],
        :solutions #{{:y 0, :z 1, :x 1}
                     {:y 1, :z 1, :x 1}
                     {:y 1, :z 1, :x 0}
                     {:y 0, :z 0, :x 0}}}
       )
      )

    (testing "set"
      (testing "with empty sets allowed"
        (test-loco
         [
          ($set :set #{} #{0 1 2 3})
          ($int :max [0 1])
          ($max :set :max false)]
         {:model '[[:var :set :public [:set #{} #{0 1 2 3}]]
                   [:var :max :public [:bool 0 1]]
                   [max [:max [of :set] [not-empty? false]]]],
          :compiled [["set = [{}, {0, 1, 2, 3}]" "max = [0,1]"]
                     ["SETMAX ([PropMaxElement(set, max)])"]],
          :solutions #{
                       {:set #{1},   :max 1}
                       {:set #{},    :max 1}
                       {:set #{0},   :max 0}
                       {:set #{0 1}, :max 1}
                       {:set #{},    :max 0}}}
         ))

      (testing "without empty sets"
        (test-loco
         [
          ($set :set #{} #{0 1 2 3})
          ($int :max [0 1])
          ($max :set :max true)]
         {:compiled [["set = [{}, {0, 1, 2, 3}]" "max = [0,1]"]
                     ["SETMAX ([PropNotEmpty(set), PropMaxElement(set, max)])"]],
          :solutions
          #{
            {:set #{1},   :max 1}
            {:set #{0},   :max 0}
            {:set #{0 1}, :max 1}}}
         ))

      (testing "indices"
        (testing "with empty sets allowed"
          (test-loco
           [
            ($set :indicies #{} #{0 1 2 3 4})
            ($int :max 0 5)
            ($max :indicies [1 2 3 5 8] 0 :max false)]
           {:model '[[:var :indicies :public [:set #{} #{0 1 2 3 4}]]
                     [:var :max :public [:int 0 5]]
                     [max [:max [of [1 2 3 5 8]] [indices :indicies] [offset 0] [not-empty? false]]]],
            :compiled [["indicies = [{}, {0, 1, 2, 3, 4}]" "max = {0..5}"]
                       ["SETMAX ([PropMaxElement(indicies, max)])"]],
            :solutions #{
                         {:indicies #{0},     :max 1} {:indicies #{0 1 3 2}, :max 5}
                         {:indicies #{},      :max 1} {:indicies #{1 3 2},   :max 5}
                         {:indicies #{},      :max 3} {:indicies #{0 1 2},   :max 3}
                         {:indicies #{0 2},   :max 3} {:indicies #{1 2},     :max 3}
                         {:indicies #{3},     :max 5} {:indicies #{0 3 2},   :max 5}
                         {:indicies #{},      :max 4} {:indicies #{0 1},     :max 2}
                         {:indicies #{1},     :max 2} {:indicies #{0 3},     :max 5}
                         {:indicies #{},      :max 5} {:indicies #{},        :max 2}
                         {:indicies #{0 1 3}, :max 5} {:indicies #{},        :max 0}
                         {:indicies #{1 3},   :max 5} {:indicies #{2},       :max 3}
                         {:indicies #{3 2},   :max 5}}}
           ))

        (testing "with empty sets"
          (test-loco
           [
            ($set :indicies #{} #{0 1 2 3 4})
            ($int :max 0 5)
            ($max :indicies [1 2 3 5 8] 0 :max true)]
           {:model '[[:var :indicies :public [:set #{} #{0 1 2 3 4}]]
                     [:var :max :public [:int 0 5]]
                     [max [:max [of [1 2 3 5 8]] [indices :indicies] [offset 0] [not-empty? true]]]],
            :compiled [["indicies = [{}, {0, 1, 2, 3, 4}]" "max = {0..5}"]
                       ["SETMAX ([PropNotEmpty(indicies), PropMaxElement(indicies, max)])"]],
            :solutions #{
                         {:indicies #{0},     :max 1} {:indicies #{0 1 3 2}, :max 5}
                         {:indicies #{1 3 2}, :max 5} {:indicies #{0 1 2},   :max 3}
                         {:indicies #{0 2},   :max 3} {:indicies #{1 2},     :max 3}
                         {:indicies #{3},     :max 5} {:indicies #{0 3 2},   :max 5}
                         {:indicies #{0 1},   :max 2} {:indicies #{1},       :max 2}
                         {:indicies #{0 3},   :max 5} {:indicies #{0 1 3},   :max 5}
                         {:indicies #{1 3},   :max 5} {:indicies #{2},       :max 3}
                         {:indicies #{3 2},   :max 5}}}
           ))))))
