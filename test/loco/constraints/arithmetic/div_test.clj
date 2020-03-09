(ns loco.constraints.arithmetic.div-test
  (:require
   [clojure.test :refer :all]
   [loco.constraints :refer :all]
   [loco.constraints.test-utils :refer :all]
   ))

(deftest div-test
  (test-loco
   [($in :x 0 2)
    ($in :y 0 2)
    ($in :z 0 2)
    ($div :z :x :y)]
   {:model
    '[[:var :x :public [:int 0 2]]
      [:var :y :public [:int 0 2]]
      [:var :z :public [:int 0 2]]
      [div [:z = :x / :y]]],
    :compiled
    [["x = {0..2}" "y = {0..2}" "z = {0..2}"]
     ["DIVISION ([PropDivXYZ(x, y, z, ..., z)])"]],
    :solutions
    #{{:x 1, :y 1, :z 1} {:x 2, :y 1, :z 2} {:x 2, :y 2, :z 1}
      {:x 1, :y 2, :z 0} {:x 0, :y 1, :z 0} {:x 0, :y 2, :z 0}}}
   )

  (is (thrown? AssertionError ($div 0 0 0)))
  )

;; (ns loco.constraints.div-test
;;   (:require
;;    [loco.model :as model]
;;    [loco.compiler :as compiler]
;;    [loco.solver :as solver]
;;    [loco.constraints.test-utils :refer :all])
;;   (:use
;;    loco.constraints
;;    clojure.test))

;; (deftest ^:model model-test
;;   (are [expected input] (= expected (->> input model/compile))
;;     [[:var :x :public [:int 0 10]]
;;      [:var :y :public [:int 0 10]]
;;      [:var :z :public [:int 0 10]]
;;      [:constraint ['div [:z '= :x '/ :y]]]]
;;     [($in :x 0 10)
;;      ($in :y 0 10)
;;      ($in :z 0 10)
;;      ($div :z :x :y)]

;;     [[:var :x :public [:int 0 10]]
;;      [:var :y :public [:int 0 10]]
;;      [:var :z :public [:int 0 10]]
;;      [:constraint ['div [:z '= :x '/ :y]]]]
;;     [($in :x 0 10)
;;      ($in :y 0 10)
;;      ($in :z 0 10)
;;      ($div :z = :x / :y)]
;;     )
;;   )

;; (deftest ^:compiler compile-test
;;   (are [expected input] (= expected (utils/constraints-strings input))
;;     '("DIVISION ([PropDivXYZ(x, y, 0, ..., cste -- 0)])")
;;     [($in :x 5 5)
;;      ($in :y 0 2)
;;      ($div 0 :x :y)]
;;     )
;;   )

;; (deftest ^:solutions solution-test
;;   (are [expected input] (= expected (solver/solutions input))
;;     '({:x 1, :y -1, :z -1}
;;       {:x 2, :y -2, :z -1}
;;       {:x 2, :y -1, :z -2}
;;       {:x 3, :y -1, :z -3}
;;       {:x 3, :y -3, :z -1}
;;       {:x 3, :y -2, :z -1}
;;       {:x 4, :y -4, :z -1}
;;       {:x 4, :y -1, :z -4}
;;       {:x 4, :y -3, :z -1}
;;       {:x 4, :y -2, :z -2}
;;       {:x 5, :y -1, :z -5}
;;       {:x 5, :y -2, :z -2}
;;       {:x 5, :y -3, :z -1}
;;       {:x 5, :y -4, :z -1}
;;       {:x 5, :y -5, :z -1})
;;     [($in :x 1 5)
;;      ($in :y -5 -1)
;;      ($in :z -5 -1)
;;      ($div :z :x :y)]
;;     )
;;   )
