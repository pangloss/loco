(in-ns 'loco.constraints)
(ns loco.constraints.tree
  (:use loco.constraints.utils)
  (:require
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :as utils]
   [loco.match :refer [match+]]
   [clojure.core.match :refer [match]]
   [clojure.walk :as walk])
  (:import
   [org.chocosolver.solver.variables SetVar IntVar BoolVar Task]))

(def ^:private constraint-name 'tree)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/tuple
                       (s/coll-of int-var?)
                       (s/tuple #{'nb-trees} int-var?)
                       (s/tuple #{'offset} nat-int?)))))

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:args [succs [_ nb-trees] [_ offset]]}
           (.tree model (into-array IntVar succs) nb-trees offset)

           ::s/invalid
           (report-spec-error constraint-name ::compile-spec var-subed-statement))))

(defn $tree
  "Creates a tree constraint.
  Partition succs variables into nbTrees (anti) arborescences
  succs[i] = j means that j is the successor of i.
  and succs[i] = i means that i is a root
  dominator-based filtering: Fages & Lorca (CP'11)
  However, the filtering over nbTrees is quite light here"
  {:choco ["tree(IntVar[] succs, IntVar nbTrees)"
           "tree(IntVar[] succs, IntVar nbTrees, int offset)"]
   :gccat "http://sofdem.github.io/gccat/gccat/Ctree.html"}
  ([succs, nb-trees] ($tree succs nb-trees 0))
  ([succs, nb-trees, offset]
   {:pre [(nat-int? offset) (sequential? succs)]}
   (constraint constraint-name
               [(vec succs)
                ['nb-trees nb-trees]
                ['offset (preserve-consts offset)]]
               compiler)))
