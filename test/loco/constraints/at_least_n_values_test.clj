(ns loco.constraints.at-least-n-values-test
  (:require
   [clojure.test :refer :all]
   [loco.constraints :refer :all]
   [loco.constraints.test-utils :refer :all]
   ))

(deftest at-least-n-values-test
  (test-loco
   [($in :a 0 2)
    ($in :b 0 2)
    ($in :n-values 2)
    ($at-least-n-values [:a :b] :n-values)
    ($at-least-n-values [:a :b] 2 true)]
   {:model
    '[[:var :a :public [:int 0 2]]
      [:var :b :public [:int 0 2]]
      [:var :n-values :public [:int 2 2]]
      [at-least-n-values
       [[:a :b] [n-values :n-values] [ac false]]]
      [at-least-n-values [[:a :b] [n-values 2] [ac true]]]],
    :compiled
    [["a = {0..2}" "b = {0..2}" "n-values = 2"]
     ["ATLEASTNVALUES ([PropAtLeastNValues(a, b, n-values)])"
      "ATLEASTNVALUES ([PropAtLeastNValues(a, b, cste -- 2), PropAtLeastNValues_AC(a, b, cste -- 2)])"]],
    :solutions
    #{{:a 0, :b 2, :n-values 2} {:a 1, :b 0, :n-values 2}
      {:a 2, :b 0, :n-values 2} {:a 1, :b 2, :n-values 2}
      {:a 2, :b 1, :n-values 2} {:a 0, :b 1, :n-values 2}}}
   )

  )
