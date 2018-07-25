(ns loco.constraints.views.scale-test
  (:require
   [clojure.test :refer :all]
   [loco.constraints :refer :all]
   [loco.constraints.test-utils :refer :all]
   ))

(deftest scale-test
  (is
   (loco?
    [($in :a -5 10)
     ($= :a ($scale 2 3))]
    {:identity
     '[[:var :a :public [:int -5 10]]
       [arithm [:a = [scale 2 [3]]]]],
     :model
     '[[:var :a :public [:int -5 10]]
       [:view "2*3" [scale 2 [3]] [:int 6 6]]
       [arithm [:a = "2*3"]]],
     :compiled [["a = {-5..10}"
                 "cste -- 6 = 6"]
                ["ARITHM ([a = 6])"]],
     :solutions #{{:a 6}}})
   )

  (is
   (loco?
    [($in :a -5 10)
     ($= 6 ($scale :a 3))]
    {:identity '[[:var :a :public [:int -5 10]]
                 [arithm [6 = [scale :a [3]]]]],
     :model '[[:var :a :public [:int -5 10]]
              [:view "a*3" [scale :a [3]] [:int -15 30]]
              [arithm [6 = "a*3"]]],
     :compiled [["a = {-5..10}"
                 "(a = {-5..10} * 3) = [-15,30]"]
                ["ARITHM ([(a*3) = 6])"]],
     :solutions #{{:a 2}}})
   )

  (testing "edge cases as described in docs"
    (is (thrown? AssertionError ($scale 2 -3)))
    (is (thrown? AssertionError ($scale :b :a)))
    (is (loco?
         [($in :a -5 10)
          ($= :a ($scale 2 0))
          ]
         {:model '[[:var :a :public [:int -5 10]]
                   [:view "2*0" [scale 2 [0]] [:int 0 0]]
                   [arithm [:a = "2*0"]]]
          :compiled [["a = {-5..10}"
                      "cste -- 0 = 0"]
                     ["ARITHM ([a = 0])"]],,
          :solutions #{{:a 0}}}))
    (is (loco?
         [($in :a -5 10)
          ($= :a ($scale 2 -1))
          ]
         {:model '[[:var :a :public [:int -5 10]]
                   [:view "-(2)" [scale 2 [-1]] [:int -2 -2]]
                   [arithm [:a = "-(2)"]]]
          :compiled [["a = {-5..10}"
                      "cste -- -2 = -2"]
                     ["ARITHM ([a = -2])"]],
          :solutions #{{:a -2}}}))
    (is (loco?
         [($in :a -5 10)
          ($= :a ($scale 2 1))
          ]
         {:model '[[:var :a :public [:int -5 10]]
                   [:view "2*1" [scale 2 [1]] [:int 2 2]]
                   [arithm [:a = "2*1"]]]
          :compiled [["a = {-5..10}"
                      "cste -- 2 = 2"]
                     ["ARITHM ([a = 2])"]],
          :solutions #{{:a 2}}}))
    )

  )
