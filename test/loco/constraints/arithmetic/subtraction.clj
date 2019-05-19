;; TODO: complete subtraction test
(ns loco.constraints.arithmetic.subtraction-test
  (:require
   [clojure.test :refer :all]
   [loco.constraints :refer :all]
   [loco.constraints.test-utils :refer :all]
   ))

;; convert below to subtraction

;; (deftest addition-partial-test
;;   (testing "partial vars name generation"
;;     (is
;;      (loco?
;;       [($in :g 0 5)
;;        ($in :h 0 5)
;;        ($in :j 1 2)
;;        ($= 12 ($+ :g :h :j 0))]
;;       {:model '[[:var :g :public [:int 0 5]]
;;                 [:var :h :public [:int 0 5]]
;;                 [:var :j :public [:int 1 2]]
;;                 [:var "g+h+j+0" :proto [:int 1 12]]
;;                 [sum ["g+h+j+0" = [:g :h :j 0]]]
;;                 [arithm [12 = "g+h+j+0"]]]}
;;       ))
;;     )

;;   (testing "should compress and use offset for raw numbers"
;;     (is (loco?
;;          [($in :x 0 5)
;;           ($= :x ($+ 1 1))]
;;          {:model '[[:var :x :public [:int 0 5]]
;;                    [arithm [:x = 2]]],
;;           :solutions #{{:x 2}}}))

;;     (is (loco?
;;          [($in :x 0 5)
;;           ($in :y -5 0)
;;           ($= :x ($+ :y 1))]
;;          {:model
;;           '[[:var :x :public [:int 0 5]]
;;             [:var :y :public [:int -5 0]]
;;             [:view "y+1" [offset :y [1]] [:int -4 1]]
;;             [arithm [:x = "y+1"]]],
;;           :solutions #{{:x 1, :y 0} {:x 0, :y -1}}}))

;;     (is (loco?
;;          [($in :x 0 5)
;;           ($in :y -5 0)
;;           ($= :x ($+ :y 1 2 3))]
;;          {:model
;;           '[[:var :x :public [:int 0 5]]
;;             [:var :y :public [:int -5 0]]
;;             [:view "y+6" [offset :y [6]] [:int 1 6]]
;;             [arithm [:x = "y+6"]]],
;;           :solutions
;;           #{{:x 1, :y -5} {:x 5, :y -1} {:x 3, :y -3} {:x 2, :y -4}
;;             {:x 4, :y -2}}}))

;;     )

;;   (testing "should handle 0 arity calls gracefully"
;;     (is (loco?
;;          [($in :x 0 1)
;;           ($in :y -1 0)
;;           ($= :x ($+ ))]
;;          {:model
;;           '[[:var :x :public [:int 0 1]]
;;             [:var :y :public [:int -1 0]]]
;;           :solutions
;;           #{{:x 0, :y 0} {:x 1, :y 0} {:x 0, :y -1} {:x 1, :y -1}}
;;           }
;;          )))

;;   (is (loco?
;;        [($in :x 0 5)
;;         ($in :y -5 0)
;;         ($= :x ($+ :y))]
;;        {:model
;;         '[[:var :x :public [:int 0 5]]
;;           [:var :y :public [:int -5 0]]
;;           [arithm [:x = :y]]],
;;         :solutions #{{:x 0, :y 0}}}))

;;   (is (loco?
;;        [($in :x 0 5)
;;         ($in :y -5 0)
;;         ($= :x ($+ ($+ :y :y)))]
;;        {:model
;;         '[[:var :x :public [:int 0 5]]
;;           [:var :y :public [:int -5 0]]
;;           [:var "y+y" :proto [:int -10 0]]
;;           [sum ["y+y" = [:y :y]]]
;;           [arithm [:x = "y+y"]]]}
;;        ))

;;   (is (loco?
;;        [($in :x 0 5)
;;         ($in :y -5 0)
;;         ($= :x ($+ ($+ :y :y 0)))]
;;        {:model
;;         '[[:var :x :public [:int 0 5]]
;;           [:var :y :public [:int -5 0]]
;;           [:var "y+y+0" :proto [:int -10 0]]
;;           [sum ["y+y+0" = [:y :y 0]]]
;;           [arithm [:x = "y+y+0"]]]}
;;        ))

;;   (testing "addition should handle 1 arity calls gracefully"
;;     (is (loco?
;;          [($in :x 0 5)
;;           ($in :y -5 0)
;;           ($= :x ($+ :y))]
;;          {:model
;;           '[[:var :x :public [:int 0 5]]
;;             [:var :y :public [:int -5 0]]
;;             [arithm [:x = :y]]],
;;           :solutions #{{:x 0, :y 0}}})))

;;   (is
;;    (loco?
;;     [($in :g 0 5)
;;      ($in :h 0 5)
;;      ($in :j 1 2)
;;      ($= 12 ($+ :g :h :j 0))]
;;     {:identity
;;      '[[:var :g :public [:int 0 5]]
;;        [:var :h :public [:int 0 5]]
;;        [:var :j :public [:int 1 2]]
;;        [arithm [12 = [+ [:g :h :j 0]]]]],
;;      :model
;;      '[[:var :g :public [:int 0 5]]
;;        [:var :h :public [:int 0 5]]
;;        [:var :j :public [:int 1 2]]
;;        [:var "g+h+j+0" :proto [:int 1 12]]
;;        [sum ["g+h+j+0" = [:g :h :j 0]]]
;;        [arithm [12 = "g+h+j+0"]]],
;;      :compiled
;;      [["g = {0..5}"
;;        "h = {0..5}"
;;        "j = {1..2}"
;;        "g+h+j+0 = {1..12}"]
;;       ["SUM ([j + h + g - g+h+j+0 = 0])"
;;        "ARITHM ([g+h+j+0 = 12])"]],
;;      :solutions #{{:g 5, :h 5, :j 2}}}
;;     ))

;;   (is
;;    (loco?
;;     [($in :x 0 5)
;;      ($in :y 0 5)
;;      ($in :z 1 2)
;;      ($= 10 ($+ :y ($+ :x :z)))]
;;     {:identity
;;      '[[:var :x :public [:int 0 5]]
;;        [:var :y :public [:int 0 5]]
;;        [:var :z :public [:int 1 2]]
;;        [arithm [10 = [+ [:y [+ [:x :z]]]]]]],
;;      :model
;;      '[[:var :x :public [:int 0 5]]
;;        [:var :y :public [:int 0 5]]
;;        [:var :z :public [:int 1 2]]
;;        [:var "x+z" :proto [:int 1 7]]
;;        [:var "y+x+z" :proto [:int 1 12]]
;;        [sum ["x+z" = [:x :z]]]
;;        [sum ["y+x+z" = [:y "x+z"]]]
;;        [arithm [10 = "y+x+z"]]],
;;      :compiled
;;      [["x = {0..5}"
;;        "y = {0..5}"
;;        "z = {1..2}"
;;        "x+z = {1..7}"
;;        "y+x+z = {1..12}"]
;;       ["SUM ([PropXplusYeqZ(x, z, x+z)])"
;;        "SUM ([PropXplusYeqZ(y, x+z, y+x+z)])"
;;        "ARITHM ([y+x+z = 10])"]],
;;      :solutions
;;      #{{:x 3, :y 5, :z 2} {:x 4, :y 4, :z 2} {:x 4, :y 5, :z 1}
;;        {:x 5, :y 3, :z 2} {:x 5, :y 4, :z 1}}}
;;     ))

;;   )


;; (ns loco.constraints.arithmetic.subtraction
;;   (:use loco.constraints clojure.test))

;; #_(deftest subtraction-output-test
;;   (are [expected input] (= expected input)
;;     [[:var :x :public [:int 0 5]]
;;      [:var :y :public [:int 0 5]]
;;      [:var "-y" :proto [:int -5 0]] ;; should this be a view?
;;      [:constraint ['sum [:x '= ["-y"]]]]]
;;     [($in :x 0 5)
;;      ($in :y 0 5)
;;      ($= :x ($- :y))]

;;     ;; [[:var :x :public [:int 0 5]]
;;     ;;  [:var :y :public [:int 0 5]]
;;     ;;  [:var :10 :hidden [:const 10]]
;;     ;;  [:var :5 :hidden [:const 5]]
;;     ;;  [:constraint ['sum [:10 '= [:x :y :5]]]]]
;;     ;; [($in :x 0 5)
;;     ;;  ($in :y 0 5)
;;     ;;  ($= 10 ($- :y :x 5))]

;;     ;;TODO: maybe it's better to make a boolean namespace instead of doing crazy overloading
;;     ;;not converted, not sure if it should be...
;;     ;; [[:var :x :public [:bool 0 1]]
;;     ;;  [:var :y :public [:bool 0 1]]
;;     ;;  [:var :z :public [:bool 0 1]]
;;     ;;  [:var :1 :hidden [:const 1]]
;;     ;;  [:constraint ['sum [:1 '= [:x :y :z]]]]]
;;     ;; [($bool :x)
;;     ;;  ($bool :y)
;;     ;;  ($bool :z)
;;     ;;  ($sum 1 := [:x :y :z])]

;;     ;;TODO: maybe it's better to put this into a set namespace, and not do crazy overloading like this. it would make for better documentation, at the least... maybe
;;     ;; [($in :sum 0 10)
;;     ;;  ($set :set [0 1 2] [0 1 2 3 4 5 6 7])
;;     ;;  ($sum :sum :set)]

;;     )
;;   )
