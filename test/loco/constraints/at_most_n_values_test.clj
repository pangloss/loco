(ns loco.constraints.at-most-n-values-test
  (:require
   [clojure.test :refer :all]
   [loco.constraints :refer :all]
   [loco.constraints.test-utils :refer :all]
   ))

(deftest at-most-n-values-test
  (is
   (loco?
    [($in :a 0 2)
     ($in :b 0 2)
     ($in :n-values 0 2)
     ($at-most-n-values [:a :b] :n-values)]
    {:model
     '[[:var :a :public [:int 0 2]]
       [:var :b :public [:int 0 2]]
       [:var :n-values :public [:int 0 2]]
       [at-most-n-values
        [[:a :b] [n-values :n-values] [strong false]]]],
     :compiled
     [["a = {0..2}" "b = {0..2}" "n-values = {0..2}"]
      ["ATMOSTNVALUES ([PropAtMostNValues(a, b, n-values)])"]],
     :solutions
     #{{:a 0, :b 2, :n-values 2} {:a 1, :b 1, :n-values 2}
       {:a 1, :b 1, :n-values 1} {:a 0, :b 0, :n-values 1}
       {:a 1, :b 0, :n-values 2} {:a 0, :b 0, :n-values 2}
       {:a 2, :b 0, :n-values 2} {:a 2, :b 2, :n-values 2}
       {:a 1, :b 2, :n-values 2} {:a 2, :b 2, :n-values 1}
       {:a 2, :b 1, :n-values 2} {:a 0, :b 1, :n-values 2}}}
    ))

  (is
   (loco?
    [($in :a 0 2)
     ($in :b 0 2)
     ($at-most-n-values [:a :b] 1)]
    {:solutions #{{:a 0, :b 0} {:a 1, :b 1} {:a 2, :b 2}}}
    ))

  (is
   (loco?
    [($in :a 0 2)
     ($in :b 0 2)
     ($at-most-n-values [:a :b 2] 1)]
    {:solutions #{{:a 2, :b 2}}}
    ))

  (is
   (loco?
    [($in :a 0 2)
     ($in :b 0 2)
     ($in :n-values 0 2)
     ($at-most-n-values [:a :b] :n-values true)]
    {:model
     '[[:var :a :public [:int 0 2]]
       [:var :b :public [:int 0 2]]
       [:var :n-values :public [:int 0 2]]
       [at-most-n-values
        [[:a :b] [n-values :n-values] [strong true]]]],
     :compiled
     [["a = {0..2}" "b = {0..2}" "n-values = {0..2}"]
      ["ATMOSTNVALUES ([PropAtMostNValues(a, b, n-values), PropAMNV(a, b, n-values)])"]]}
    ))

  )
