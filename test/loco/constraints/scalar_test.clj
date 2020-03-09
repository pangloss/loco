;;FIXME: do some scalar tests
(ns loco.constraints.scalar-test
  (:require
   [loco.integer.sendmoremoney1-test :as sendmoney]
   [clojure.test :refer :all]
   [loco.constraints :refer :all]
   [loco.constraints.test-utils :refer :all]
   ))

(def simple-model [($in :x 1 5)
                   ($in :y 1 5)
                   ($in :z 1 5)
                   ($in :ans [315 111 555])
                   ($= :ans ($scalar [[100 :x] [10 :y] [1 :z]]))])

(deftest scalar-test
  (testing "simple model"
    (is
     (test-loco
      simple-model
      {
       :identity '[[:var :x :public [:int 1 5]]
                   [:var :y :public [:int 1 5]]
                   [:var :z :public [:int 1 5]]
                   [:var :ans :public [:int [111 315 555]]]
                   [arithm [:ans = [scalar [[100 :x] [10 :y] [1 :z]]]]]]
       :model    '[[:var :x :public [:int 1 5]]
                   [:var :y :public [:int 1 5]]
                   [:var :z :public [:int 1 5]]
                   [:var :ans :public [:int [111 315 555]]]
                   [:var "scalar_100x+10y+1z" :proto [:int 111 555]]
                   [scalar ["scalar_100x+10y+1z" = [[100 :x] [10 :y] [1 :z]]]]
                   [arithm [:ans = "scalar_100x+10y+1z"]]]
       :compiled [["x = {1..5}"
                   "y = {1..5}"
                   "z = {1..5}"
                   "ans = {111,315,555}"
                   "scalar_100x+10y+1z = {111..555}"]
                  ["TABLE ([CSPLarge({x = {1..5}, , y = {1..5}, , z = {1..5}, , scalar_100x+10y+1z = {111..555}, })])"
                   "ARITHM ([prop(ans.EQ.scalar_100x+10y+1z)])"]]
       :solutions #{{:x 5, :y 5, :z 5, :ans 555}
                    {:x 3, :y 1, :z 5, :ans 315}
                    {:x 1, :y 1, :z 1, :ans 111}}
       })))

  (testing "send money"
    (is
     (test-loco
      sendmoney/model
      {
       :identity
       '[[:var :S :public [:int 0 9]]
         [:var :E :public [:int 0 9]]
         [:var :N :public [:int 0 9]]
         [:var :D :public [:int 0 9]]
         [:var :M :public [:int 0 9]]
         [:var :O :public [:int 0 9]]
         [:var :R :public [:int 0 9]]
         [:var :Y :public [:int 0 9]]
         [all-different [:S :E :N :D :M :O :R :Y]]
         [arithm [:S > 0]]
         [arithm [:M > 0]]
         [arithm [[+
                   [[scalar [[1000 :S] [100 :E] [10 :N] [1 :D]]]
                    [scalar [[1000 :M] [100 :O] [10 :R] [1 :E]]]]]
                  =
                  [scalar [[10000 :M] [1000 :O] [100 :N] [10 :E] [1 :Y]]]]]],
       :model
       '[[:var :S :public [:int 0 9]]
         [:var :E :public [:int 0 9]]
         [:var :N :public [:int 0 9]]
         [:var :D :public [:int 0 9]]
         [:var :M :public [:int 0 9]]
         [:var :O :public [:int 0 9]]
         [:var :R :public [:int 0 9]]
         [:var :Y :public [:int 0 9]]
         [:var "scalar_1000S+100E+10N+1D" :proto [:int 0 9999]]
         [:var "scalar_1000M+100O+10R+1E" :proto [:int 0 9999]]
         [:var "scalar_1000S+100E+10N+1D+scalar_1000M+100O+10R+1E" :proto [:int 0 19998]]
         [:var "scalar_10000M+1000O+100N+10E+1Y" :proto [:int 0 99999]]
         [all-different [:S :E :N :D :M :O :R :Y]]
         [arithm [:S > 0]]
         [arithm [:M > 0]]
         [scalar ["scalar_1000S+100E+10N+1D" = [[1000 :S] [100 :E] [10 :N] [1 :D]]]]
         [scalar ["scalar_1000M+100O+10R+1E" = [[1000 :M] [100 :O] [10 :R] [1 :E]]]]
         [sum ["scalar_1000S+100E+10N+1D+scalar_1000M+100O+10R+1E"
           = ["scalar_1000S+100E+10N+1D" "scalar_1000M+100O+10R+1E"]]]
         [scalar ["scalar_10000M+1000O+100N+10E+1Y" = [[10000 :M] [1000 :O] [100 :N] [10 :E] [1 :Y]]]]
         [arithm ["scalar_1000S+100E+10N+1D+scalar_1000M+100O+10R+1E" = "scalar_10000M+1000O+100N+10E+1Y"]]],
       :compiled
       [["S = {0..9}"
         "E = {0..9}"
         "N = {0..9}"
         "D = {0..9}"
         "M = {0..9}"
         "O = {0..9}"
         "R = {0..9}"
         "Y = {0..9}"
         "scalar_1000S+100E+10N+1D = {0..9999}"
         "scalar_1000M+100O+10R+1E = {0..9999}"
         "scalar_1000S+100E+10N+1D+scalar_1000M+100O+10R+1E = {0..19998}"
         "scalar_10000M+1000O+100N+10E+1Y = [0,99999]"]
        ["ALLDIFFERENT ([PropAllDiffInst(S, E, N, ..., Y), PropAllDiffBC(S, E, N, ..., Y), PropAllDiffAdaptative(S, E, N, ..., Y)])"
         "ARITHM ([S >= 1])"
         "ARITHM ([M >= 1])"
         "SUM ([1000.S + 100.E + 10.N + 1.D - 1.scalar_1000S+100E+10N+1D = 0])"
         "SUM ([1000.M + 100.O + 10.R + 1.E - 1.scalar_1000M+100O+10R+1E = 0])"
         "SUM ([PropXplusYeqZ(scalar_1000S+100E+10N+1D, scalar_1000M+100O+10R+1E, scalar_1000S+100E+10N+1D+scalar_1000M+100O+10R+1E)])"
         "SUM ([10000.M + 1000.O + 100.N + 10.E + 1.Y - 1.scalar_10000M+1000O+100N+10E+1Y = 0])"
         "ARITHM ([prop(scalar_1000S+100E+10N+1D+scalar_1000M+100O+10R+1E.EQ.scalar_10000M+1000O+100N+10E+1Y)])"]],
       :solutions #{{:S 9, :E 5, :N 6, :D 7, :M 1, :O 0, :R 8, :Y 2}}})))
  )
