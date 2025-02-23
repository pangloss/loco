(ns ^{:author "Andrea Richiardi"
      :doc "Send + More = Money example solution #1 + tests. This solution associate a var with a
            letter in the domain (SENDMORY), and constraints it using $scalar."
      :solutions true}
    loco.integer.sendmoremoney1-test
  (:require [clojure.test :refer :all]
            ;[loco.core :refer :all]
            [loco.constraints :refer :all]
            [loco.solver :as solver]))

(defn initialize-digits [vars]
  (for [v vars]
    ($in v 0 9)))

(def letters
  [:S :E :N :D :M :O :R :Y])

(def model
  (vec (concat (initialize-digits letters)
               [($distinct [:S :E :N :D :M :O :R :E :M :O :N :E :Y])
                ($> :S 0)
                ($> :M 0)
                ($= ($+ ($scalar [:S :E :N :D] [1000 100 10 1])
                        ($scalar [:M :O :R :E] [1000 100 10 1]))
                    ($scalar [:M :O :N :E :Y] [10000 1000 100 10 1]))])))

#_(deftest sendmoremoney-tests
  "Testing Send + More = Money solution #1 with $scalar"

  (is (= 1 (count
            (solver/solutions model)
            ))
      "Just one solution")
    (is (= (solver/solutions model)
           '({:S 9 :E 5 :N 6 :D 7 :M 1 :O 0 :R 8 :Y 2}))
      "Has correct solution"))
