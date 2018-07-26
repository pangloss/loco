(ns loco.constraints.abs-test
  (:require
   [clojure.test :refer :all]
   [loco.constraints :refer :all]
   [loco.constraints.test-utils :refer :all]
   ))

(deftest abs-test
  (is
   (loco?
    [($in :y 0 10)
     ($in :z -5 0)
     ;;($abs :y = :z)
     ($abs :y :z)
     ]
    {:identity '[[:var :y :public [:int 0 10]]
                 [:var :z :public [:int -5 0]]
                 [abs [:y = :z]]],
     :model '[[:var :y :public [:int 0 10]]
              [:var :z :public [:int -5 0]]
              [abs [:y = :z]]],
     :compiled [["y = {0..10}" "z = {-5..0}"]
                ["ABSOLUTE ([y = {0..10} = |z = {-5..0}|])"]],
     :solutions #{{:y 4, :z -4}
                  {:y 3, :z -3}
                  {:y 1, :z -1}
                  {:y 2, :z -2}
                  {:y 0, :z 0}
                  {:y 5, :z -5}}}))

  (is
   (loco?
    [($in :y 0 10)
     ($in :z -5 0)
     ($= :y ($abs :z))
     ]
    {:model
     '[[:var :y :public [:int 0 10]]
       [:var :z :public [:int -5 0]]
       [:view "|z|" [abs :z []] [:int 0 5]]
       [arithm [:y = "|z|"]]],
     :compiled
     [["y = {0..10}" "z = {-5..0}" "-(z = {-5..0}) = [0,5]"]
      ["ARITHM ([prop(y.EQ.-(z))])"]],
     :solutions
     #{{:y 4, :z -4} {:y 3, :z -3} {:y 1, :z -1} {:y 2, :z -2}
       {:y 0, :z 0} {:y 5, :z -5}}})
   "partial should be supported")

  (is
   (loco?
    [($in :y 0 10)
     ($in [:z] -5 0)
     ($= :y ($abs ($+ [:z] 4)))
     ]
    {:model
     '[[:var :y :public [:int 0 10]]
       [:var "[:z]" :public [:int -5 0]]
       [:view "[:z]+4" [offset "[:z]" [4]] [:int -1 4]]
       [:view "|[:z]+4|" [abs "[:z]+4" []] [:int 1 4]]
       [arithm [:y = "|[:z]+4|"]]],
     :compiled
     [["y = {0..10}"
       "[:z] = {-5..0}"
       "([:z] = {-5..0} + 4) = [-1,4]"
       "|([:z]+4)| = {0..4}"]
      ["ABSOLUTE ([|([:z]+4)| = {0..4} = |([:z] = {-5..0} + 4) = [-1,4]|])"
       "ARITHM ([prop(y.EQ.|([:z]+4)|)])"]],
     :solutions
     #{{:y 1, [:z] -3} {:y 3, [:z] -1} {:y 1, [:z] -5}
       {:y 4, [:z] 0} {:y 0, [:z] -4} {:y 2, [:z] -2}}})
   "partial should be supported")

  )
