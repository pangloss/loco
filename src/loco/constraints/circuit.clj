(in-ns 'loco.constraints)
(ns loco.constraints.circuit
  (:use loco.constraints.utils)
  (:require
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :as utils]
   [loco.match :refer [match+]]
   [clojure.core.match :refer [match]]
   [clojure.walk :as walk])
  (:import
   org.chocosolver.solver.constraints.nary.circuit.CircuitConf
   [org.chocosolver.solver.variables SetVar IntVar BoolVar]))

(def ^:private constraint-name 'circuit)

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
                              (s/tuple #{'conf} #{'all 'first 'light 'rd}))))))

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:args [:no-conf [vars [ _ offset]]]}
           (.circuit model (into-array IntVar vars) offset)

           {:args [:conf [vars [ _ offset] [_ conf]]]}
           (.circuit model
                     (into-array IntVar vars)
                     offset
                     ({
                       'all   CircuitConf/ALL
                       'first CircuitConf/FIRST
                       'light CircuitConf/LIGHT
                       'rd    CircuitConf/RD
                       } conf))

           ::s/invalid
           (report-spec-error constraint-name ::compile-spec var-subed-statement))))

(defn $circuit
  "Given a list of int-vars L, and an optional offset number (default
  0), the elements of L define a circuit, where (L[i] = j + offset)
  means that j is the successor of i.  Hint: make the offset 1 when
  using a 1-based list.

  vars = IntVar[]
  offset = integer, default=0
  circuit-conf of #{:all, :first, :light, :rd}

  Filtering algorithms:
  - subtour elimination : Caseau & Laburthe (ICLP'97)
  - allDifferent GAC algorithm: Régin (AAAI'94)
  - dominator-based filtering: Fages & Lorca (CP'11)
  - Strongly Connected Components based filtering (Cambazard & Bourreau JFPC'06 and Fages and Lorca TechReport'12)

  See Fages PhD Thesis (2014) for more information"
  {:choco ["circuit(IntVar[] vars)"
           "circuit(IntVar[] vars, int offset)"
           "circuit(IntVar[] vars, int offset, CircuitConf conf)"]}
  ([vars]
   ($circuit vars 0))
  ([vars offset]
   {:pre [(nat-int? offset) (sequential? vars)]}
   (constraint constraint-name
               [(vec vars)
                ['offset (preserve-consts offset)]]
               compiler))

  ([vars offset circuit-conf]
   {:pre [(nat-int? offset)
          (sequential? vars)
          (#{:all :first :light :rd 'all 'first 'light 'rd} circuit-conf)]}
   (constraint constraint-name
               [(vec vars)
                ['offset (preserve-consts offset)]
                ['conf (symbol circuit-conf)]]
               compiler)))
