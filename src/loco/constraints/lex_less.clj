(ns loco.constraints.lex-less

  (:require
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :refer :all :as utils]

   [meander.epsilon :as m :refer [match]]
   [clojure.walk :as walk])
  (:import
   [org.chocosolver.solver.variables SetVar IntVar BoolVar Task]))

(def ^:private constraint-name 'lex-less)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/tuple (s/coll-of int-var?) #{'lex-of} (s/coll-of int-var?)))))

(compile-function compiler constraint-name [*conformed *model]
 (match *conformed
   {:args [?vars _ ?lex-less-or-equal-vars]}
   (.lexLess *model
             (into-array IntVar ?vars)
             (into-array IntVar ?lex-less-or-equal-vars))))

(defn $lex-less
  "Creates a lexLessEq constraint.
  Ensures that vars1 is lexicographically less or equal than vars2."
  {:choco "lexLess(IntVar[] vars1, IntVar[] vars2)"}
  [vars, lex-less-or-equal-vars]
  {:pre [(sequential? vars) (sequential? lex-less-or-equal-vars)]}
  (constraint constraint-name
              [(vec vars) 'lex-of (vec lex-less-or-equal-vars)]
              compiler))
