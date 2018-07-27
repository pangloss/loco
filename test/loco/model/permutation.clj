(ns ^:model loco.model.permutation
  (:use clojure.test
        loco.model.test
        loco.constraints))

;;http://sofdem.github.io/gccat/gccat/Kpermutation.html#uid8618

(deftest distinct-test
  (compiled-assert
   [[:var :a :public [:int 0 9]]
    [:var :b :public [:int 0 9]]
    [:var :c :public [:int 0 9]]
    [:constraint [:distinct [:a :b :c]]]]

   [($in :a 0 9)
    ($in :b 0 9)
    ($in :c 0 9)
    ($distinct :a :b :c)])

  (compiled-assert
   [[:var :a :public [:int 0 9]]
    [:var :b :public [:int 0 9]]
    [:var :c :public [:int 0 9]]
    [:constraint [:distinct [:a :b :c]]]]

   [($in :a 0 9)
    ($in :b 0 9)
    ($in :c 0 9)
    ($distinct [:a :b :c])])

  (compiled-assert
   [[:var :a :public [:int 0 9]]
    [:var :b :public [:int 0 9]]
    [:var :c :public [:int 0 9]]
    [:constraint [:distinct [:a :b :c] [:consistency :ac]]]]

   [($in :a 0 9)
    ($in :b 0 9)
    ($in :c 0 9)
    ($distinct [:a :b :c] :ac)])
)
