(ns loco.constraints.automata.regular
  (:require
   [clojure.core.match :refer [match]]
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

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))
        coerce-int-var (p utils/coerce-int-var model)]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:args [vars [_ automation]]}
           (.regular model
                     (->> vars (map coerce-int-var) (into-array IntVar))
                     automation)

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
