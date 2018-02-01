(in-ns 'loco.constraints)
(ns loco.constraints.element
  (:use loco.constraints.utils)
  (:require
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :as utils]
   [loco.match :refer [match+]]
   [clojure.core.match :refer [match]]
   [clojure.walk :as walk])
  (:import
   [org.chocosolver.solver.variables SetVar IntVar BoolVar Task]))

(def ^:private constraint-name 'element)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/or
                       :ints (s/tuple
                              int-var?
                              (s/tuple #{'in}     (s/coll-of int-var?))
                              (s/tuple #{'at}     int-var?)
                              (s/tuple #{'offset} nat-int?))
                       :sets (s/tuple
                              set-var?
                              (s/tuple #{'in}     (s/coll-of set-var?))
                              (s/tuple #{'at}     int-var?)
                              (s/tuple #{'offset} nat-int?))))))

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:args [(:or :ints :sets) [value [_ vars] [_ index] [_ offset]]]}
           (.element model value (into-array vars) index offset)

           ::s/invalid
           (report-spec-error constraint-name ::compile-spec var-subed-statement))))

(defn $element
  "-------------------- IntVar --------------------
  Given a list of int-vars L, an int-var i, and an optional offset
  number (default 0), returns a new int-var constrained to equal L[i],
  or L[i - offset].

  value    - an integer variable taking its value in table
  table    - an array of integer values or variables
  index    - an integer variable representing the value of value in table
  offset   - offset matching index.lb and table[0] (Generally 0)

  -------------------- SetVar --------------------
  Creates a constraint enabling to retrieve an element set in sets: sets[index-offset] = set
  Creates a constraint enabling to retrieve an element set in sets: sets[index] = set

  value    - a set variable taking its value in table
  table    - an array of set vars
  index    - an integer variable representing the value of value in table
  offset   - offset matching index.lb and table[0] (Generally 0)"
  {:choco ["element(IntVar value, int[] table, IntVar index, int offset)"
           "element(IntVar value, IntVar[] table, IntVar index, int offset)"
           "element(IntVar index, SetVar[] sets, SetVar set)"
           "element(IntVar index, SetVar[] sets, int offset, SetVar set)"]}
  ([value table index]
   ($element value table index 0))

  ([value table index offset]
   {:pre [(nat-int? offset) (sequential? table)]}
   (let [table (if (every? int? table)
                 (preserve-consts table)
                 table)]
     (constraint constraint-name
                 [value
                  ['in table]
                  ['at index]
                  ['offset (preserve-consts offset)]]
                 compiler))))

(def $elem $element)
(reset-meta! (var $elem) (meta (var $element)))
