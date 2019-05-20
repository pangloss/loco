(ns loco.constraints.arithmetic.subtraction-test
  (:require
   [clojure.test :refer :all]
   [loco.constraints :refer :all]
   [loco.constraints.test-utils :refer :all]
   ))

;;TODO: move this test into some other namespace
#_(deftest subtration-meta-test
  (is (=
       {
        :doc "partial of $sum\n\n  e.g.:\n  ($= :eq ($- :n1 :n2 :n3 4)) => ($sum :eq := :n1 -:n2 -:n3 -4)\n  ",
        :partial true,
        :file "loco/constraints/arithmetic/subtraction.clj",
        }
       (select-keys (meta $-) [:doc :partial :file])) ;;:arglists ([& args]), in future
      "documentation and other meta data on the loco.constraints symbol is preserved"
      )
  )

(deftest subtration-partial-test

  (testing "should compress and use offset for raw numbers"
    (is (loco?
         [($in :x 0 5)
          ($= :x ($- 1 1))]
         {:model
          '[[:var :x :public [:int 0 5]]
            [arithm [:x = 0]]]}
         ))

    (is (loco?
         [($in :x 0 5)
          ($in :y -5 0)
          ($= :x ($- :y 1))]
         {:model
          '[[:var :x :public [:int 0 5]]
            [:var :y :public [:int -5 0]]
            [:view "y-1" [offset :y [-1]] [:int -6 -1]]
            [arithm [:x = "y-1"]]]}))
    
    (is (loco?
         [($in :x 0 5)
          ($in :y -5 0)
          ($= :x ($- :y 1 2 3))]
         {:model
          '[[:var :x :public [:int 0 5]]
            [:var :y :public [:int -5 0]]
            [:view "y-6" [offset :y [-6]] [:int -11 -6]]
            [arithm [:x = "y-6"]]]}
         ))

    )

  (testing "should handle 0 arity calls gracefully"
    (is (loco?
         [($in :x 0 1)
          ($in :y -1 0)
          ($= :x ($- ))]
         {:model
          '[[:var :x :public [:int 0 1]]
            [:var :y :public [:int -1 0]]]
          :solutions
          #{{:x 0, :y 0} {:x 1, :y 0} {:x 0, :y -1} {:x 1, :y -1}}
          }
         )))

  (testing "should handle 1 arity calls"
    (is (loco?
         [($in :x 0 5)
          ($in :y -5 0)
          ($= :x ($- :y))]
         {:identity '[[:var :x :public [:int 0 5]]
                      [:var :y :public [:int -5 0]]
                      [arithm [:x = [- [:y]]]]],
          :model '[[:var :x :public [:int 0 5]]
                   [:var :y :public [:int -5 0]]
                   [:view "-y" [minus :y []] [:int 0 5]]
                   [arithm [:x = "-y"]]],
          :compiled [["x = {0..5}"
                      "y = {-5..0}"
                      "-(y = {-5..0}) = [0,5]"]
                     ["ARITHM ([prop(x.EQ.-(y))])"]],
          :solutions #{{:x 0, :y 0}
                       {:x 5, :y -5}
                       {:x 4, :y -4}
                       {:x 1, :y -1}
                       {:x 2, :y -2}
                       {:x 3, :y -3}}})))

  (is
   (loco?
    [($in :g 0 5)
     ($in :h 0 5)
     ($in :j 1 2)
     ($= 0 ($- :g :h :j 1))]
    {:identity
     '[[:var :g :public [:int 0 5]]
       [:var :h :public [:int 0 5]]
       [:var :j :public [:int 1 2]]
       [arithm [0 = [- [:g :h :j 1]]]]],
     :model
     '[[:var :g :public [:int 0 5]]
       [:var :h :public [:int 0 5]]
       [:var :j :public [:int 1 2]]
       [:var "g-h-j-1" :proto [:int -8 3]]
       [:view "-h" [minus :h []] [:int -5 0]]
       [:view "-j" [minus :j []] [:int -2 -1]]
       [sum ["g-h-j-1" = [:g "-h" "-j" -1]]]
       [arithm [0 = "g-h-j-1"]]],
     :compiled
     [["g = {0..5}"
       "h = {0..5}"
       "j = {1..2}"
       "g-h-j-1 = {-8..3}"
       "-(h = {0..5}) = [-5,0]"
       "-(j = {1..2}) = [-2,-1]"]
      ["SUM ([-(j) + -(h) + g - g-h-j-1 = 1])"
       "ARITHM ([g-h-j-1 = 0])"]],
     :solutions
     #{{:g 4, :h 2, :j 1} {:g 4, :h 1, :j 2} {:g 5, :h 2, :j 2}
       {:g 3, :h 0, :j 2} {:g 3, :h 1, :j 1} {:g 2, :h 0, :j 1}
       {:g 5, :h 3, :j 1}}}))

  (is
   (loco?
    [($in :x 0 5)
     ($in :y 0 5)
     ($in :z 1 2)
     ($= :x ($- :y ($- :x :z)))]
    {:identity
     '[[:var :x :public [:int 0 5]]
       [:var :y :public [:int 0 5]]
       [:var :z :public [:int 1 2]]
       [arithm [:x = [- [:y [- [:x :z]]]]]]],
     :model
     '[[:var :x :public [:int 0 5]]
       [:var :y :public [:int 0 5]]
       [:var :z :public [:int 1 2]]
       [:var "x-z" :proto [:int -2 4]]
       [:view "-z" [minus :z []] [:int -2 -1]]
       [:var "y-x-z" :proto [:int -4 7]]
       [:view "-x-z" [minus "x-z" []] [:int -4 2]]
       [sum ["x-z" = [:x "-z"]]]
       [sum ["y-x-z" = [:y "-x-z"]]]
       [arithm [:x = "y-x-z"]]],
     :compiled
     [["x = {0..5}"
       "y = {0..5}"
       "z = {1..2}"
       "x-z = {-2..4}"
       "-(z = {1..2}) = [-2,-1]"
       "y-x-z = {-4..7}"
       "-(x-z = {-2..4}) = [-4,2]"]
      ["SUM ([PropXplusYeqZ(x, -(z), x-z)])"
       "SUM ([PropXplusYeqZ(y, -(x-z), y-x-z)])"
       "ARITHM ([prop(x.EQ.y-x-z)])"]],
     :solutions
     #{{:x 1, :y 1, :z 1} {:x 3, :y 5, :z 1} {:x 2, :y 3, :z 1}
       {:x 2, :y 2, :z 2} {:x 3, :y 4, :z 2} {:x 1, :y 0, :z 2}}}
    ))

  (is
   (loco?
    [($in :a 0 5)
     ($in :b 0 5)
     ($= ($minus :a) ($- :b))]
    {:identity
     '[[:var :a :public [:int 0 5]]
       [:var :b :public [:int 0 5]]
       [arithm [[minus :a []] = [- [:b]]]]],
     :model
     '[[:var :a :public [:int 0 5]]
       [:var :b :public [:int 0 5]]
       [:view "-a" [minus :a []] [:int -5 0]]
       [:view "-b" [minus :b []] [:int -5 0]]
       [arithm ["-a" = "-b"]]],
     :compiled
     [["a = {0..5}"
       "b = {0..5}"
       "-(a = {0..5}) = [-5,0]"
       "-(b = {0..5}) = [-5,0]"]
      ["ARITHM ([prop(-(a).EQ.-(b))])"]],
     :solutions
     #{{:a 4, :b 4} {:a 0, :b 0} {:a 1, :b 1} {:a 5, :b 5}
       {:a 3, :b 3} {:a 2, :b 2}}}
    ))
  )
