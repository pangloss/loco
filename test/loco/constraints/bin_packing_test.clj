(ns loco.constraints.bin-packing-test
  (:require
   [clojure.test :refer :all]
   [loco.constraints :refer :all]
   [loco.constraints.test-utils :refer :all]
   )
  )

(deftest bin-packing-test
  (is
   (loco?
    [($in :bin-load 0 10)
     ($bin-packing
      {0 5}
      [:bin-load]
      )]
    {:solutions #{{:bin-load 5}}}
    ))

  (is
   (loco?
    [($in :item-bin 0 10)
     ($in :item-bin2 0 10)
     ($in :bin-load 0 10)
     ($bin-packing
      {:item-bin 5
       :item-bin2 4}
      [:bin-load]
      )]
    {:solutions #{{:item-bin 0, :item-bin2 0, :bin-load 9}}}
    ))

  (is
   (loco?
    [($in :item-bin 0 10)
     ($in :item-bin2 0 10)
     ($in :bin-load 0 10)
     ($in :bin-load2 0 5)
     ($bin-packing
      {:item-bin 5
       :item-bin2 4}
      [:bin-load :bin-load2]
      )]
    {:solutions
     #{{:item-bin 0, :item-bin2 1, :bin-load 5, :bin-load2 4}
       {:item-bin 1, :item-bin2 0, :bin-load 4, :bin-load2 5}
       {:item-bin 0, :item-bin2 0, :bin-load 9, :bin-load2 0}}}
    ))

  (is
   (loco?
    [($in :i1-bin 0 2)
     ($in :i2-bin 0 2)
     ($in :i3-bin 0 2)
     ($in :bin-load-1 0 2)
     ($in :bin-load-2 0 5)
     ($bin-packing
      {:i1-bin 1
       :i2-bin 2
       :i3-bin 3}
      [:bin-load-1 :bin-load-2]
      )]
    {:model
     '[[:var :i1-bin :public [:int 0 2]]
       [:var :i2-bin :public [:int 0 2]]
       [:var :i3-bin :public [:int 0 2]]
       [:var :bin-load-1 :public [:int 0 2]]
       [:var :bin-load-2 :public [:int 0 5]]
       [bin-packing
        [[item-bin [:i1-bin :i2-bin :i3-bin]]
         [item-size [1 2 3]]
         [bin-load [:bin-load-1 :bin-load-2]]
         [offset 0]]]],
     :compiled
     [["i1-bin = {0..2}"
       "i2-bin = {0..2}"
       "i3-bin = {0..2}"
       "bin-load-1 = {0..2}"
       "bin-load-2 = {0..5}"]
      [(str "BINPACKING ([PropItemToLoad(i1-bin, i2-bin, i3-bin, ..., bin-load-2),"
            " PropLoadToItem(bin-load-1, bin-load-2, i1-bin, ..., i3-bin),"
            " bin-load-1 = {0..2} + bin-load-2 = {0..5} = 6])")]],
     :solutions
     #{{:i1-bin 0,
        :i2-bin 1,
        :i3-bin 1,
        :bin-load-1 1,
        :bin-load-2 5}
       {:i1-bin 1,
        :i2-bin 0,
        :i3-bin 1,
        :bin-load-1 2,
        :bin-load-2 4}}}
    ))

  )
