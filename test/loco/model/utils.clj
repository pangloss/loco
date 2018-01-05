(ns ^:model loco.model.utils
  (:use loco.constraints.utils
        clojure.test)
  (:require [clojure.core.match :refer [match]]))

(deftest preserver-const-test

  (def num-const (preserve-consts 0))
  (def num-const-const (preserve-consts (preserve-consts 0)))
  (def keyword-const (preserve-consts :a))
  (def vector-const (preserve-consts [1 2 3]))
  (def vector-const-const (preserve-consts (preserve-consts [1 2 3])))

  (is (= num-const [0])
      "a number const will be wrapped in an array")
  (is (= (meta num-const) {:preserve-const true}))

  (is (= num-const-const [0])
      "a recursively called number const should be a passthrough")
  (is (= (meta num-const-const) {:preserve-const true}))

  (is (= keyword-const :a))
  (is (= (meta keyword-const) nil) "non-numeric or vectors are passthroughs")

  (is (= vector-const [1 2 3]))
  (is (= (meta vector-const) {:preserve-consts true}))

  (is (= vector-const-const [1 2 3])
      "recursively called vector const is a passthrough")
  (is (= (meta vector-const-const) {:preserve-consts true}))
  )
