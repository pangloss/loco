;;FIXME: do some scalar tests
(ns loco.constraints.scalar-test
  (:require
   [clojure.test :refer :all]
   [loco.constraints :refer :all]
   [loco.constraints.test-utils :refer :all]
   ))

;; this is just copied from arithm tests, needs fixing
;; TODO:
(deftest scalar-test
  (testing
    (is
     (loco?
      [($in :x 1 5)
       ($in :y 1 5)
       ($in :z 1 5)
       ($in :ans [315 111 555])
       ($= :ans ($scalar [[100 :x] [10 :y] [1 :z]]))]
      {
       :identity [[:var :x :public [:int 1 5]]
                  [:var :y :public [:int 1 5]]
                  [:var :z :public [:int 1 5]]
                  ],

       :model    [[:var :x :public [:int 1 5]]
                  [:var :y :public [:int 1 5]]
                  [:var :z :public [:int 1 5]]
                  [:var :ans :public [:int 111 555]]]

       :compiled [["y = {0..10}"] []]

       :solutions #{{:x 3 :y 1 :z 5}
                    {:x 1 :y 1 :z 1}
                    {:x 5 :y 5 :z 5}}

       })))
  )
