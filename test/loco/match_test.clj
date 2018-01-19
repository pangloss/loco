(ns loco.match-test
  (:use loco.match
        clojure.test)
  (:require [clojure.core.match :refer [match]]))

(deftest match+test
  (is (=
       (match  1
               a 2)
       (match+ 1
               a 2)))

  (is (= (match  1
                 a a)
         (match+ 1
                 a a)))

  (is (= (match  1
                 (b :guard integer?) :return)
         (match+ 1
                 b :guard [b integer?] :return)))

  (is (= (match  {:d 1}
                 {:d (d :guard integer?)} d)
         (match+ {:d 1}
                 {:d d} :guard [d [integer?]] d)))

  (is (= (match  {:d {:a 1}}
                 {:d {:a (d :guard integer?)}} d)
         (match+ {:d {:a 1}}
                 {:d {:a d}} :guard [d integer?] d)))

  (is (= (match  [1 [1]]
                 [(a :guard integer?) [(b :guard integer?)]] a)
         (match+ [1 [1]]
                 [a [b]] :guard [a integer?
                                 b integer?] a)))

  (is (= (match  [1 1 2 2 ]
                 [(a :guard integer?) (b :guard integer?)
                  (c :guard [integer? even?]) (d :guard [integer? even?])] a)
         (match+ [1 1 2 2]
                 [a b c d] :guard [[a b] integer?
                                   [c d] [integer? even?]] a))
      "should be able to express shared guards")

  )
