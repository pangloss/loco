(ns loco.constraints.tree

  (:require
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :refer :all :as utils]

   [meander.epsilon :as m :refer [match]]
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

(compile-function
 (match *conformed
   {:args [?succs [_ ?nb-trees] [_ ?offset]]}
   (.tree *model (into-array IntVar ?succs) ?nb-trees ?offset)))

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
                ['offset  offset]]
               compiler)))
