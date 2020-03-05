(ns loco.constraints.sub-path

  (:require
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :refer :all :as utils]

   [meander.epsilon :as m :refer [match]]
   [clojure.walk :as walk])
  (:import
   [org.chocosolver.solver.variables SetVar IntVar BoolVar Task]))

(def ^:private constraint-name 'sub-path)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/tuple (s/coll-of int-var?)
                               (s/tuple #{'start}  int-var?)
                               (s/tuple #{'end}    int-var?)
                               (s/tuple #{'offset} nat-int?)
                               (s/tuple #{'size}   int-var?)))))

(compile-function
 (match *conformed
   {:args [?vars [_ ?start] [_ ?end] [_ ?offset] [_ ?size]]}
   (.subPath *model (into-array IntVar ?vars) ?start ?end ?offset ?size)))

(defn $sub-path
  "Creates a subPath constraint which ensures that
  the elements of vars define a path of SIZE vertices, leading from start to end
  where vars[i] = offset+j means that j is the successor of i.
  where vars[i] = offset+i means that vertex i is excluded from the path.
  Moreover, vars[end-offset] = |vars|+offset
  Requires : |vars|>0

  Filtering algorithms: see subCircuit constraint"
  {:choco "subPath(IntVar[] vars, IntVar start, IntVar end, int offset, IntVar SIZE)"}
  ([vars start end size] ($sub-path vars start end size 0))
  ([vars start end size offset]
   {:pre [(sequential? vars) (nat-int? offset) (pos? (count vars))]}
   (constraint constraint-name
               [(vec vars)
                ['start start]
                ['end end]
                ['offset  offset]
                ['size size]]
               compiler)))
