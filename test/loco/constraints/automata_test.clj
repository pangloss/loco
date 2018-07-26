(ns loco.constraints.automata-test
  (:require
   [clojure.test :refer :all]
   [loco.automata :as a]
   [loco.constraints :refer :all]
   [loco.constraints.test-utils :refer :all]
   [loco.solver :as solver]
   ))

;;holy crap, there are like 1000 assertions in this test
(deftest automaton-test
  (def automations [["string->automaton"
                     (a/string->automaton "12*3+")]
                    ["map->automaton"
                     (a/map->automaton {:q0 {1 :q1}
                                        :q1 {2 :q1
                                             3 :q2}
                                        :q2 {3 :q2}}
                                       :q0 #{:q2})]
                    ["make-automaton"
                     (a/make-automaton [:q0 :q1 :q2]
                                       [[:q0 :q1 1]
                                        [:q1 :q1 2]
                                        [:q1 :q2 3]
                                        [:q2 :q2 3]]
                                       :q0 [:q2])]
                    ["Union two automata"
                     (a/union (a/string->automaton "12?3+")
                              (a/string->automaton "12+3+"))]
                    ["Concatenate automata"
                     (reduce a/cat
                             [(a/string->automaton "1")
                              (a/string->automaton "2*")
                              (a/string->automaton "3+")])]
                    ["Intersect automata"
                     (a/intersection (a/string->automaton "12*4*3+")
                                     (a/string->automaton "12*5?3+"))]
                    ["Minimized automaton via Hopcroft"
                     (-> (a/union (a/string->automaton "12?3+")
                                  (a/string->automaton "12+3+"))
                         (a/minimize!))]
                    ["Minimized automaton via Brzozowski"
                     (-> (a/union (a/string->automaton "12?3+")
                                  (a/string->automaton "12+3+"))
                         (a/minimize! :brzozowski))]
                    ["Minimized automaton via Huffman"
                     (-> (a/union (a/string->automaton "12?3+")
                                  (a/string->automaton "12+3+"))
                         (a/minimize! :huffman))]])

  (doseq [[description automaton] automations]
    (testing (str description)
      (are [x y] (= (set y) (set (solver/solutions x)))
        [($in :x 1 5)
         ($in :y 1 5)
         ($in :z 1 5)
         ($regular automaton [:x :y :z])]
        [{:x 1 :y 2 :z 3}
         {:x 1 :y 3 :z 3}]

        [($in :x 1 5)
         ($regular automaton [:x])]
        [])))

  (doseq [[description automaton] automations]
    (doseq [input [[1 3]
                   [1 2 2 3]
                   [1 3 3]
                   [1 2 3 3]
                   [1 2 2 3 3]]]
      (is (= '({}) (solver/solutions [($regular automaton input)]))
          (str "Input " input " satisfies automaton constraint"))
      (is (= true (a/run automaton input))
          (str "Input " input " satisfies automaton"))))

  (doseq [[description automaton] automations]
    (doseq [input [[1]
                   ;; [] ; doesn't work, see https://github.com/chocoteam/choco3/issues/335
                   [1 2 3 4]
                   [1 3 2]
                   [1 2]
                   [1 2 2]
                   [2 2 3 3]]]
      (is (empty? (solver/solutions [($regular automaton input)]))
          (str "Input " input " doesn't satisfy automaton constraint"))
      (is (= false (a/run automaton input))
          (str "Input " input " doesn't satisfy automaton"))))

  )
