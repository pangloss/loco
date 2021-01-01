(ns loco.constraints.circuit
  (:require
   [meander.epsilon :as m :refer [match]]
   [clojure.spec.alpha :as s]
   [clojure.walk :as walk]
   [loco.constraints.utils :refer :all :as utils]
   )
  (:import
   org.chocosolver.solver.constraints.nary.circuit.CircuitConf
   [org.chocosolver.solver.variables
    SetVar
    IntVar
    BoolVar]))

(def ^:private constraint-name 'circuit)

(def ^:private conf-map
  {
   :all   CircuitConf/ALL
   :first CircuitConf/FIRST
   :light CircuitConf/LIGHT
   :rd    CircuitConf/RD
   })

(def ^:private allowed-conf-values
  (set (keys conf-map)))

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/or
                       :no-conf (s/tuple
                                 (s/coll-of int-var?)
                                 (s/tuple #{'offset} nat-int?))
                       :conf (s/tuple
                              (s/coll-of int-var?)
                              (s/tuple #{'offset} nat-int?)
                              (s/tuple #{'conf} allowed-conf-values))))))

(compile-function compiler constraint-name [*conformed *model]
 (match *conformed
   {:args [:no-conf [?vars [_ ?offset]]]}
   (.circuit *model (into-array IntVar ?vars) ?offset)

   {:args [:conf [?vars [_ ?offset] [_ ?conf]]]}
   (.circuit *model
             (into-array IntVar ?vars)
             ?offset
             (conf-map ?conf))

   {:args [:conf [?vars [_ ?offset] [_ ?conf]]]}
   (.circuit *model
             (into-array IntVar ?vars)
             ?offset
             ({
               'all   CircuitConf/ALL
               'first CircuitConf/FIRST
               'light CircuitConf/LIGHT
               'rd    CircuitConf/RD
               } ?conf))))

;;TODO: can implement a version that takes in something more looking like a graph, [[:a :b] [:a :c]]...
(defn $circuit
  "Given a list of int-vars L, and an optional offset number (default
  0), the elements of L define a circuit, where (L[i] = j + offset)
  means that j is the successor of i.

  Hint: make the offset 1 when using a 1-based list.

  vars = IntVar[]
  offset = integer, default=0
  circuit-conf of #{:all, :first, :light, :rd}

  Filtering algorithms:
    - subtour elimination:        Caseau & Laburthe (ICLP'97)
    - allDifferent GAC algorithm: Régin (AAAI'94)
    - dominator-based filtering:  Fages & Lorca (CP'11)
    - Strongly Connected Components based filtering"
  {:choco ["circuit(IntVar[] vars)"
           "circuit(IntVar[] vars, int offset)"
           "circuit(IntVar[] vars, int offset, CircuitConf conf)"]
   :gccat ["http://sofdem.github.io/gccat/gccat/Ccircuit.html"]}
  ([vars]
   ($circuit vars 0))
  ([vars offset]
   {:pre [(nat-int? offset) (sequential? vars)]}
   (constraint constraint-name
               [(vec vars)
                ['offset offset]]
               compiler))

  ([vars offset circuit-conf]
   {:pre [(nat-int? offset)
          (sequential? vars)
          (allowed-conf-values circuit-conf)]}
   (constraint constraint-name
               [(vec vars)
                ['offset offset]
                ['conf (symbol circuit-conf)]]
               compiler)))
