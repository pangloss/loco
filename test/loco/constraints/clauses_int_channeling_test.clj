(ns loco.constraints.clauses-int-channeling-test
  (:require
   [clojure.test :refer :all]
   [loco.constraints :refer :all]
   [loco.constraints.test-utils :refer :all]
   ))

(deftest clauses-int-channeling-test
  (is
   (loco?
    [($bools :a1 :a2 :a3 :b1 :b2 :b3)
     ($in :var 0 2)
     ($clauses-int-channeling :var [:a1 :a2 :a3] [:b1 :b2 :b3])]
    {
     :model
     '[[:var :var :public [:int 0 2]]
       [:var :a1 :public [:bool 0 1]]
       [:var :a2 :public [:bool 0 1]]
       [:var :a3 :public [:bool 0 1]]
       [:var :b1 :public [:bool 0 1]]
       [:var :b2 :public [:bool 0 1]]
       [:var :b3 :public [:bool 0 1]]
       [clauses-int-channeling
        [:var [e-vars [:a1 :a2 :a3]] [l-vars [:b1 :b2 :b3]]]]],
     :compiled
     [["var = {0..2}"
       "a1 = [0,1]"
       "a2 = [0,1]"
       "a3 = [0,1]"
       "b1 = [0,1]"
       "b2 = [0,1]"
       "b3 = [0,1]"]
      ["CLAUSESINTCHANNELING ([PropClauseChanneling(var, a1, a2, ..., b3)])"]],
     :solutions
     #{
       {:var 0, :a1 1, :a2 0, :a3 0, :b1 1, :b2 1, :b3 1}
       {:var 1, :a1 0, :a2 1, :a3 0, :b1 0, :b2 1, :b3 1}
       {:var 2, :a1 0, :a2 0, :a3 1, :b1 0, :b2 0, :b3 1}
       }}
    ))


  (is
   (loco?
    [($bools :a1 :a2 :a3 :b1 :b2 :b3)
     ($in :var 0 2)
     ($clauses-int-channeling :var [:a1 :a2 :a3] [:b1 :b2 :b3])]
    {:solutions
     #{{:var 0, :a1 1, :a2 0, :a3 0, :b1 1, :b2 1, :b3 1}
       {:var 1, :a1 0, :a2 1, :a3 0, :b1 0, :b2 1, :b3 1}
       {:var 2, :a1 0, :a2 0, :a3 1, :b1 0, :b2 0, :b3 1}}}))

  (is
   (loco?
    [($bools :a1 :a2  :b1 :b2 )
     ($in :var 1 2)
     ($clauses-int-channeling :var [:a1 :a2 ] [:b1 :b2 ])]
    {:solutions
     #{{:var 1, :a1 1, :a2 0, :b1 1, :b2 1}
       {:var 2, :a1 0, :a2 1, :b1 0, :b2 1}}}))

  )
