(ns loco.constraints.bits-int-channeling-test
  (:require
   [clojure.test :refer :all]
   [loco.constraints :refer :all]
   [loco.constraints.test-utils :refer :all]
   ))

(deftest bits-int-channeling-test
  (test-loco
   [($in :int-var [0 2 4 8 17])
    ($bits-int-channeling [:b1 :b2 :b3 :b4] :int-var)]
   {:solutions
    #{{:int-var 4, :b1 0, :b2 0, :b3 1, :b4 0}
      {:int-var 8, :b1 0, :b2 0, :b3 0, :b4 1}
      {:int-var 0, :b1 0, :b2 0, :b3 0, :b4 0}
      {:int-var 2, :b1 0, :b2 1, :b3 0, :b4 0}}}
   )

  (test-loco
   [($in :int-var 0 16)
    ($bits-int-channeling [:b1 :b2 :b3 :b4] :int-var)]
   {:model
    '[[:var :int-var :public [:int 0 16]]
      [:var :b1 :public [:bool 0 1]]
      [:var :b2 :public [:bool 0 1]]
      [:var :b3 :public [:bool 0 1]]
      [:var :b4 :public [:bool 0 1]]
      [bits-int-channeling
       [[bits [:b1 :b2 :b3 :b4]] [int-var :int-var]]]],
    :compiled
    [["int-var = {0..16}"
      "b1 = [0,1]"
      "b2 = [0,1]"
      "b3 = [0,1]"
      "b4 = [0,1]"]
     ["BITSINTCHANNELING ([PropBitChanneling(int-var, b1, b2, ..., b4)])"]],
    :solutions
    #{
      {:int-var 0, :b1 0, :b2 0, :b3 0, :b4 0}
      {:int-var 1, :b1 1, :b2 0, :b3 0, :b4 0}
      {:int-var 2, :b1 0, :b2 1, :b3 0, :b4 0}
      {:int-var 3, :b1 1, :b2 1, :b3 0, :b4 0}
      {:int-var 4, :b1 0, :b2 0, :b3 1, :b4 0}
      {:int-var 5, :b1 1, :b2 0, :b3 1, :b4 0}
      {:int-var 6, :b1 0, :b2 1, :b3 1, :b4 0}
      {:int-var 7, :b1 1, :b2 1, :b3 1, :b4 0}
      {:int-var 8, :b1 0, :b2 0, :b3 0, :b4 1}
      {:int-var 9, :b1 1, :b2 0, :b3 0, :b4 1}
      {:int-var 10, :b1 0, :b2 1, :b3 0, :b4 1}
      {:int-var 11, :b1 1, :b2 1, :b3 0, :b4 1}
      {:int-var 12, :b1 0, :b2 0, :b3 1, :b4 1}
      {:int-var 13, :b1 1, :b2 0, :b3 1, :b4 1}
      {:int-var 14, :b1 0, :b2 1, :b3 1, :b4 1}
      {:int-var 15, :b1 1, :b2 1, :b3 1, :b4 1}
      }}
   )

  )
