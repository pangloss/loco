(ns loco.constraints.bools-int-channeling-test
  (:require
   [clojure.test :refer :all]
   [loco.constraints :refer :all]
   [loco.constraints.test-utils :refer :all]
   ))

(deftest bools-int-channeling-test
  (is
   (loco?
    [($bools :a :b :c :d)
     ($in :int 0 4)
     ($bools-int-channeling [:a :b :c :d] :int)]
    {:model
     '[[:var :a :public [:bool 0 1]]
       [:var :b :public [:bool 0 1]]
       [:var :c :public [:bool 0 1]]
       [:var :d :public [:bool 0 1]]
       [:var :int :public [:int 0 4]]
       [bools-int-channeling
        [[bool-vars [:a :b :c :d]] [int-var :int] [offset 0]]]],
     :compiled
     [["a = [0,1]"
       "b = [0,1]"
       "c = [0,1]"
       "d = [0,1]"
       "int = {0..4}"]
      ["BOOLCHANNELING ([PropEnumDomainChanneling(a, b, c, ..., int)])"]],
     :solutions
     #{
       {:a 1, :b 0, :c 0, :d 0, :int 0}
       {:a 0, :b 1, :c 0, :d 0, :int 1}
       {:a 0, :b 0, :c 1, :d 0, :int 2}
       {:a 0, :b 0, :c 0, :d 1, :int 3}
       }}
    )
   )

  (is
   (loco?
    [($bools :a :b :c)
     ($in :int 0 4)
     ($bools-int-channeling [:a :b :c 1] :int)]
    {:model
     '[[:var :a :public [:bool 0 1]]
       [:var :b :public [:bool 0 1]]
       [:var :c :public [:bool 0 1]]
       [:var :int :public [:int 0 4]]
       [bools-int-channeling
        [[bool-vars [:a :b :c 1]] [int-var :int] [offset 0]]]],
     :compiled
     [["a = [0,1]" "b = [0,1]" "c = [0,1]" "int = {0..4}"]
      ["BOOLCHANNELING ([PropEnumDomainChanneling(a, b, c, ..., int)])"]],
     :solutions #{{:a 0, :b 0, :c 0, :int 3}}}
    )
   )

  )
