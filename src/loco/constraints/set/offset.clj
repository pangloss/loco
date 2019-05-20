(ns loco.constraints.set.offset
  (:use loco.constraints.utils)
  (:require
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :as utils]
   [loco.match :refer [match+]]
   [clojure.core.match :refer [match]]
   [clojure.walk :as walk])
  (:import
   [org.chocosolver.solver.variables SetVar IntVar BoolVar Task]))

(def ^:private constraint-name 'set/offset)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/tuple set-var? set-var?
                               (s/tuple #{'offset} nat-int?)))))

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:args [set1 set2 ['offset offset]]}
           (.offSet model set1 set2 offset)

           ::s/invalid
           (report-spec-error constraint-name ::compile-spec var-subed-statement))))

(defloco $offset
  "Creates a constraint linking set1 and set2
  with an index offset : x in set1 <=> x+offset in set2"
  {:choco "offSet(SetVar set1, SetVar set2, int offset)"}
  [set1 set2 offset]
  {:pre [(nat-int? offset)]}
  (constraint constraint-name
              [set1 set2
               ['offset  offset]]
              compiler))
