(ns loco.constraints.cardinality-test
  (:require
   [clojure.test :refer :all]
   [loco.constraints :refer :all]
   [loco.constraints.test-utils :refer :all]
   ))

(deftest cardinality-test
  (test-loco
   [($in :a 0 3)
    ($in :b 0 3)     
    ($in :c 0 3)
    ($cardinality [:a :b :c] {1 2, 2 1})] ;;so cool!
   {:solutions
    #{{:a 1, :b 1, :c 2}
      {:a 2, :b 1, :c 1}
      {:a 1, :b 2, :c 1}}})

  (test-loco
   [($in :a 2 3)
    ($in :b 1 3)
    ($in :c 1 3)
    ($in :d 2 3)
    ($in :e 1 3)
    ($in :ones [0 10])
    ($cardinality [:a :b :c :d :e] {1 :ones, 2 :twos} :closed)]
   {:identity
    '[[:var :a :public [:int 2 3]]
      [:var :b :public [:int 1 3]]
      [:var :c :public [:int 1 3]]
      [:var :d :public [:int 2 3]]
      [:var :e :public [:int 1 3]]
      [:var :ones :public [:int [0 10]]]
      [[:var :ones :public [:int [0 1 2 3 4]]]
       [:var :twos :public [:int [0 1 2 3 4]]]
       [cardinality
        [[:a :b :c :d :e] {1 :ones, 2 :twos} [closed true]]]]],
    :model
    '[[:var :a :public [:int 2 3]]
      [:var :b :public [:int 1 3]]
      [:var :c :public [:int 1 3]]
      [:var :d :public [:int 2 3]]
      [:var :e :public [:int 1 3]]
      [:var :ones :public [:int [0 10]]]
      [:var :twos :public [:int [0 1 2 3 4]]]
      [cardinality
       [[:a :b :c :d :e] {1 :ones, 2 :twos} [closed true]]]]})

  (test-loco
   [($in :a 2 3)
    ($in :b 1 3)
    ($cardinality [:a :b] {1 :same, 2 :same})]
   {:solutions #{{:a 3, :b 3, :same 0}
                 {:a 2, :b 1, :same 1}}}))

;; legacy test
#_(deftest cardinality-test
    (-> (solver/solutions
         [($in :a 1 5)
          ($in :b 1 5)
          ($in :c 1 5)
          ($in :d 1 5)
          ($in :e 1 5)
          ($in :ones 1 5)
          ($in :twos 1 5)
          ($cardinality [:a :b :c :d :e] {1 :ones 2 :twos})])
        (as-> ms
            (doseq [m ms]
              (-> m
                  (map [:a :b :c :d :e])
                  frequencies
                  (map [1 2])
                  (= (map m [:ones :twos]))
                  is)))))
