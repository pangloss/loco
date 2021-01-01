(ns loco.constraints.sub-circuit

  (:require
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :refer :all :as utils]

   [meander.epsilon :as m :refer [match]]
   [clojure.walk :as walk])
  (:import
   [org.chocosolver.solver.variables SetVar IntVar BoolVar]))

(def ^:private constraint-name 'sub-circuit)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/tuple
                       (s/coll-of int-var?)
                       (s/tuple #{'sub-circuit-length} int-var?)
                       (s/tuple #{'offset} nat-int?)))))

(compile-function compiler constraint-name [*conformed *model]
 (match *conformed
   {:args [?vars [_ ?sub-circuit-length] [_ ?offset]]}
   (.subCircuit *model (into-array IntVar ?vars) ?offset ?sub-circuit-length)))

(defn $sub-circuit
  "Creates a subCircuit constraint which ensures that
  the elements of vars define a single circuit of subcircuitSize nodes where
  vars[i] = offset+j means that j is the successor of i.
  and vars[i] = offset+i means that i is not part of the circuit
  the constraint ensures that |{vars[i] =/= offset+i}| = subCircuitLength

  Filtering algorithms:
  subtour elimination : Caseau & Laburthe (ICLP'97)
  allDifferent GAC algorithm: Régin (AAAI'94)
  dominator-based filtering: Fages & Lorca (CP'11) (adaptive scheme by default, see implementation)"
  {:choco "subCircuit(IntVar[] vars, int offset, IntVar subCircuitLength)"}
  ([int-vars sub-circuit-length] ($sub-circuit int-vars sub-circuit-length 0))
  ([int-vars sub-circuit-length offset]
   {:pre [(nat-int? offset) (sequential? int-vars)]}
   (constraint constraint-name
               [(vec int-vars)
                ['sub-circuit-length sub-circuit-length]
                ['offset offset]]
               compiler)))
