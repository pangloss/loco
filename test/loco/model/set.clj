(ns ^:model loco.model.set
  (:require [loco.model :as model]
            [loco.compiler :as compiler]
            [loco.solver :as solver])
  (:use clojure.test
        loco.model.test
        loco.constraints))

(deftest set-var-model-test
  (compiled-assert
   [[:var :a :public [:set #{1 3 2}]]]
   [($set :a [1 2 3])])

  (compiled-assert
   [[:var :a :public [:set #{1 3 2} #{1 2 4 3 5}]]]
   [($set :a [1 2 3] [1 2 3 4 5])])

  (compiled-assert
   [[:var :a :hidden [:set #{1 3 2} #{1 2 4 3 5}]]]
   [($set- :a [1 2 3] [1 2 3 4 5])])
  )

(deftest set-var-compile-test
  (vars-string-assert
   '("a = {1, 2, 3}")
   [($set :a [1 2 3])])

  (vars-string-assert
   '("a = [{0, 3}, {-2, 0, 2, 3}]")
   [($set :a [0 3] [-2 0 2 3])])

  (vars-string-assert
   '("a = [{0, 3}, {-2, 0, 2, 3}]")
   [($set :a #{0,3}, #{-2,0,2,3})])

  (is (thrown?
       AssertionError
       ($set :a [0 3] [3 4 5])))

)

(deftest set-var-solutions-test
  (is (=
       '({:a #{0 -2 3 2}} {:a #{0 -2 3}} {:a #{0 3 2}} {:a #{0 3}})
       (->> [($set :a #{0,3}, #{-2,0,2,3})]
            solver/solutions))
      "each set should include the lower bound ints in it")

  (is (=
       '({:a #{0 3}})
       (->> [($set :a #{0,3})]
            solver/solutions))
      "constant set should only resolve to itself")
  )
