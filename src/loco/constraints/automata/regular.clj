(ns loco.constraints.automata.regular
  (:require
   [meander.epsilon :as m :refer [match]]
   [clojure.spec.alpha :as s]
   [clojure.walk :as walk]
   [loco.constraints.utils :refer :all :as utils]
   [loco.utils :refer [p]]
   )
  (:import
   [org.chocosolver.solver.variables SetVar IntVar BoolVar Task]
   org.chocosolver.solver.constraints.nary.automata.FA.FiniteAutomaton))

(def ^:private constraint-name 'regular)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/tuple ::utils/coll-coerce-intvar?
                               (s/tuple #{'automation} #(instance? FiniteAutomaton %))))))

(compile-function compiler constraint-name [*conformed *model]
 (let [coerce-int-var (p utils/coerce-int-var *model)]
   (match *conformed
     {:args [?vars [_ ?automation]]}
     (.regular *model
               (->> ?vars (map coerce-int-var) (into-array IntVar))
               ?automation))))

(defn $regular
  "Takes a Choco automaton object constructed by the loco.automata
  namespace, and constrains that a list of variables represents an
  input string accepted by the automaton."
  {:choco "regular(IntVar[] vars, IAutomaton automaton)"}
  [^FiniteAutomaton automaton vars]
  {:pre [(sequential? vars)]}
  (constraint constraint-name
              [(vec vars)
               ['automation automaton]]
              compiler))
