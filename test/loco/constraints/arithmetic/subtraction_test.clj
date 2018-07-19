(ns loco.constraints.arithmetic.subtraction-test
  (:require
   [loco.constraints :refer :all]
   [loco.model :as model]
   [loco.compiler :as compiler]
   [clojure.test :refer :all]
   ;;[loco.model.test :refer :all]
   [loco.constraints.test-utils :refer :all]))

      ;; [[:var :x :public [:int 0 5]]
      ;;  [:var :y :public [:int 0 5]]
      ;;  [:var :10 :hidden [:const 10]]
      ;;  [:var :5 :hidden [:const 5]]
      ;;  [:constraint ['sum [:10 '= [:x :y :5]]]]]
      ;; [($in :x 0 5)
      ;;  ($in :y 0 5)
      ;;  ($= 10 ($- :y :x 5))]

      ;;TODO: maybe it's better to make a boolean namespace instead of doing crazy overloading
      ;;not converted, not sure if it should be...
      ;; [[:var :x :public [:bool 0 1]]
      ;;  [:var :y :public [:bool 0 1]]
      ;;  [:var :z :public [:bool 0 1]]
      ;;  [:var :1 :hidden [:const 1]]
      ;;  [:constraint ['sum [:1 '= [:x :y :z]]]]]
      ;; [($bool :x)
      ;;  ($bool :y)
      ;;  ($bool :z)
      ;;  ($sum 1 := [:x :y :z])]

      ;;TODO: maybe it's better to put this into a set namespace, and not do crazy overloading like this. it would make for better documentation, at the least... maybe
      ;; [($in :sum 0 10)
      ;;  ($set :set [0 1 2] [0 1 2 3 4 5 6 7])
      ;;  ($sum :sum :set)]

(deftest subtraction-test
  (testing "$subtration"
    (are [expected input] (= expected input)
      '[[:var :x :public [:int 0 5]]
        [:var :y :public [:int 0 5]]
        [arithm [:x = [minus :y]]]]
      [($in :x 0 5)
       ($in :y 0 5)
       ($= :x ($- :y))]
      ))

  (testing "model/compile"
    (are [expected input] (= expected (model/compile input))
      '[[:var :x :public [:int 0 5]]
        [:var :y :public [:int 0 5]]
        [:view "-y" [minus :y]]
        [arithm [:x = "-y"]]]
      [($in :x 0 5)
       ($in :y 0 5)
       ($= :x ($- :y))]
      ))

  (testing "compiler/compile"
    (choco-vars-string-assert
      '[[:var :x :public [:int 0 5]]
        [:var :y :public [:int 0 5]]
        [arithm [:x = [minus :y]]]]
      [($in :x 0 5)
       ($in :y 0 5)
       ($= :x ($- :y))]
      ))
  )

(deftest subtraction-model-compile-test)
(deftest subtraction-choco-compile-test)
(deftest subtraction-solution-test)
