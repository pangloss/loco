(ns loco.constraints.views.offset-test
  (:require
   [clojure.test :refer :all]
   [loco.constraints :refer :all]
   [loco.constraints.test-utils :refer :all]
   ))

(deftest offset-test
  (test-loco
   [($in :a -5 10)
    ($= :a ($offset-view 2 3))]
   {:identity
    '[[:var :a :public [:int -5 10]]
      [arithm [:a = [offset 2 [3]]]]],
    :model
    '[[:var :a :public [:int -5 10]]
      [:view "2+3" [offset 2 [3]] [:int 5 5]]
      [arithm [:a = "2+3"]]],
    :compiled
    [["a = {-5..10}" "(cste -- 2+3) = 5"] ["ARITHM ([a = 5])"]],
    :solutions #{{:a 5}}})


  (test-loco
   [($in :a -5 10)
    ($= 6 ($offset-view :a 3))]
   {:identity
    '[[:var :a :public [:int -5 10]]
      [arithm [6 = [offset :a [3]]]]],
    :model
    '[[:var :a :public [:int -5 10]]
      [:view "a+3" [offset :a [3]] [:int -2 13]]
      [arithm [6 = "a+3"]]],
    :compiled
    [["a = {-5..10}" "(a = {-5..10} + 3) = [-2,13]"]
     ["ARITHM ([(a+3) = 6])"]],
    :solutions #{{:a 3}}})


  (testing "edge cases as described in docs"
    (is (thrown? AssertionError ($offset-view :b :a))))

  )
