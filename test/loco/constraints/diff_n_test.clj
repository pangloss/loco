(ns loco.constraints.diff-n-test
  (:require
   [clojure.test :refer :all]
   [loco.constraints :refer :all]
   [loco.constraints.test-utils :refer :all]
   ))

(deftest diff-n-test

  (is
   (loco?
    [
     ($in :x1 0 5)
     ($in :x2 0 5)
     ($in :x3 0 5)

     ($in :y1 0 5)
     ($in :y2 0 5)
     ($in :y3 0 5)

     ($in :w1 2 2)
     ($in :w2 0 3)
     ($in :w3 3 3)

     ($in :h1 2 2)
     ($in :h2 0 3)
     ($in :h3 5 5)

     ($diff-n
      [[:x1 :y1 :w1 :h1]
       [:x1 :y2 :w2 :h2]
       [:x1 :y3 :w3 :h3]]
      true)
     ]
    {:identity
     '[[:var :x1 :public [:int 0 5]]
       [:var :x2 :public [:int 0 5]]
       [:var :x3 :public [:int 0 5]]
       [:var :y1 :public [:int 0 5]]
       [:var :y2 :public [:int 0 5]]
       [:var :y3 :public [:int 0 5]]
       [:var :w1 :public [:int 2 2]]
       [:var :w2 :public [:int 0 3]]
       [:var :w3 :public [:int 3 3]]
       [:var :h1 :public [:int 2 2]]
       [:var :h2 :public [:int 0 3]]
       [:var :h3 :public [:int 5 5]]
       [diff-n
        [[rects
          [[x :x1 y :y1 w :w1 h :h1]
           [x :x1 y :y2 w :w2 h :h2]
           [x :x1 y :y3 w :w3 h :h3]]]
         [add-cumulative-reasoning true]]]],
     :compiled
     [["x1 = {0..5}"
       "x2 = {0..5}"
       "x3 = {0..5}"
       "y1 = {0..5}"
       "y2 = {0..5}"
       "y3 = {0..5}"
       "w1 = 2"
       "w2 = {0..3}"
       "w3 = 3"
       "h1 = 2"
       "h2 = {0..3}"
       "h3 = 5"]
      ["DIFFNWITHCUMULATIVE ([DIFFN([x1 = {0..5},y1 = {0..5},w1 = 2,h1 = 2],[x1 = {0..5},y2 = {0..5},w2 = {0..3},h2 = {0..3}],[x1 = {0..5},y3 = {0..5},w3 = 3,h3 = 5]), DIFFN([x1 = {0..5},y1 = {0..5},w1 = 2,h1 = 2],[x1 = {0..5},y2 = {0..5},w2 = {0..3},h2 = {0..3}],[x1 = {0..5},y3 = {0..5},w3 = 3,h3 = 5]), diffN_8 = [0,8] = min(x1 = {0..5}, x1 = {0..5}, x1 = {0..5}), diffN_7 = [0,8] = max(diffN_1 = [2,7], diffN_3 = [0,8], diffN_5 = [3,8]), PropXplusYeqZ(diffN_9, diffN_8, diffN_7), PropGraphCumulative([x1 = {0..5},w1 = 2,diffN_1 = [2,7],h1 = 2],[x1 = {0..5},w2 = {0..3},diffN_3 = [0,8],h2 = {0..3}],[x1 = {0..5},w3 = 3,diffN_5 = [3,8],h3 = 5],diffN_12 = [0,10]), PropGraphCumulative([x1 = {0..5},w1 = 2,diffN_1 = [2,7],h1 = 2],[x1 = {0..5},w2 = {0..3},diffN_3 = [0,8],h2 = {0..3}],[x1 = {0..5},w3 = 3,diffN_5 = [3,8],h3 = 5],diffN_12 = [0,10]), diffN_11 = [0,10] = min(y1 = {0..5}, y2 = {0..5}, y3 = {0..5}), diffN_10 = [0,10] = max(diffN_2 = [2,7], diffN_4 = [0,8], diffN_6 = [5,10]), PropXplusYeqZ(diffN_12, diffN_11, diffN_10), PropGraphCumulative([y1 = {0..5},h1 = 2,diffN_2 = [2,7],w1 = 2],[y2 = {0..5},h2 = {0..3},diffN_4 = [0,8],w2 = {0..3}],[y3 = {0..5},h3 = 5,diffN_6 = [5,10],w3 = 3],diffN_9 = [0,8]), PropGraphCumulative([y1 = {0..5},h1 = 2,diffN_2 = [2,7],w1 = 2],[y2 = {0..5},h2 = {0..3},diffN_4 = [0,8],w2 = {0..3}],[y3 = {0..5},h3 = 5,diffN_6 = [5,10],w3 = 3],diffN_9 = [0,8])])"]]}

    ))

  (is
   (loco?
    [
     ($in :x1 0 1)
     ($in :x2 0 1)

     ($in :y1 0 1)
     ($in :y2 0 1)

     ($in :_w1 1)
     ($in :_w2 1)

     ($in :_h1 1)
     ($in :_h2 1)

     ($diff-n
      [[:x1 :y1 :_w1 :_h1]
       [:x2 :y2 :_w2 :_h2]]
      true)
     ]
    {:solutions
     #{
       {:x1 0, :x2 0, :y1 0, :y2 1}
       {:x1 0, :x2 0, :y1 1, :y2 0}
       {:x1 0, :x2 1, :y1 0, :y2 0}
       {:x1 0, :x2 1, :y1 0, :y2 1}
       {:x1 0, :x2 1, :y1 1, :y2 0}
       {:x1 0, :x2 1, :y1 1, :y2 1}
       {:x1 1, :x2 0, :y1 0, :y2 0}
       {:x1 1, :x2 0, :y1 0, :y2 1}
       {:x1 1, :x2 0, :y1 1, :y2 0}
       {:x1 1, :x2 0, :y1 1, :y2 1}
       {:x1 1, :x2 1, :y1 0, :y2 1}
       {:x1 1, :x2 1, :y1 1, :y2 0}
       }}

    )
   "this represents 2 cubes in a 2x2 grid. for each position of 1
   cube, the other has 3, so total amount of permutations is 3*4=12.
 _ _
|_|_|
|_|_|
"
   ))
