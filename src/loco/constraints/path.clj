(ns loco.constraints.path
  (:use loco.constraints.utils)
  (:require
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :as utils]
   [loco.match :refer [match+]]
   [clojure.core.match :refer [match]]
   [clojure.walk :as walk])
  (:import
   [org.chocosolver.solver.variables SetVar IntVar BoolVar Task]))

(def ^:private constraint-name 'path)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/tuple (s/coll-of int-var?)
                               (s/tuple #{'start}  int-var?)
                               (s/tuple #{'end}    int-var?)
                               (s/tuple #{'offset} nat-int?)))))

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:args [vars [_ start] [_ end] [_ offset]]}
           (.path model (into-array IntVar vars) start end offset)

           ::s/invalid
           (report-spec-error constraint-name ::compile-spec var-subed-statement))))

(defn path
  "Creates a path constraint which ensures that
  the elements of vars define a covering path from start to end
  where vars[i] = j means that j is the successor of i.
  Moreover, vars[end] = |vars|
  Requires : |vars|>0

  Filtering algorithms: see circuit constraint"
  {:choco ["path(IntVar[] vars, IntVar start, IntVar end)"
           "path(IntVar[] vars, IntVar start, IntVar end, int offset)"]}
  ([vars start end] (path vars start end 0))
  ([vars start end offset]
   {:pre [(sequential? vars) (nat-int? offset) (pos? (count vars))]}
   (constraint constraint-name
               [(vec vars)
                ['start start]
                ['end end]
                ['offset (preserve-consts offset)]]
               compiler)))
