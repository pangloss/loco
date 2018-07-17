(ns loco.constraints.automata.regular
  (:use loco.constraints.utils)
  (:require
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :as utils]
   [loco.match :refer [match+]]
   [clojure.core.match :refer [match]]
   [clojure.walk :as walk])
  (:import
   [org.chocosolver.solver.variables SetVar IntVar BoolVar Task]
   org.chocosolver.solver.constraints.nary.automata.FA.FiniteAutomaton))

(def ^:private constraint-name 'regular)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/tuple (s/coll-of int-var?)
                               (s/tuple #{'automation} #(instance? FiniteAutomaton %))))))

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:args [vars [_ automation]]}
           (.regular model (into-array IntVar vars) automation)

           ::s/invalid
           (report-spec-error constraint-name ::compile-spec var-subed-statement))))

(defloco $regular
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
