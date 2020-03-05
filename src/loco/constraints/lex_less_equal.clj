(ns loco.constraints.lex-less-equal

  (:require
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :refer :all :as utils]

   [meander.epsilon :as m :refer [match]]
   [clojure.walk :as walk])
  (:import
   [org.chocosolver.solver.variables SetVar IntVar BoolVar Task]))

(def ^:private constraint-name 'lex-less-equal)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/tuple (s/coll-of int-var?) #{'lex-of} (s/coll-of int-var?)))))

(compile-function
 (match *conformed
   {:args [?vars _ ?lex-less-or-equal-vars]}
   (.lexLessEq *model
               (into-array IntVar ?vars)
               (into-array IntVar ?lex-less-or-equal-vars))))

(defn $lex-less-equal
  "Creates a lexLessEq constraint.
  Ensures that vars1 is lexicographically less or equal than vars2."
  {:choco "lexLessEq(IntVar[] vars1, IntVar[] vars2)"}
  [vars, lex-less-or-equal-vars]
  {:pre [(sequential? vars) (sequential? lex-less-or-equal-vars)]}
  (constraint constraint-name
              [(vec vars) 'lex-of (vec lex-less-or-equal-vars)]
              compiler))
