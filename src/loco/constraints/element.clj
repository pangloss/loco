(ns loco.constraints.element
  (:require
   [meander.epsilon :as m :refer [match]]
   [clojure.spec.alpha :as s]
   [clojure.walk :as walk]
   [loco.constraints.utils :refer :all :as utils]
   )
  (:import
   [org.chocosolver.solver.variables SetVar IntVar BoolVar Task]))

(def ^:private constraint-name 'element)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/or
                       :ints (s/tuple
                              ::utils/int-var?
                              (s/tuple #{'in}     (s/or :ints (s/coll-of int?)
                                                        :int-vars ::utils/coll-intvar?))
                              (s/tuple #{'at}     ::utils/coerce-intvar?)
                              (s/tuple #{'offset} nat-int?))
                       :sets (s/tuple
                              ::utils/set-var?
                              (s/tuple #{'in}     ::utils/coll-setvar?)
                              (s/tuple #{'at}     ::utils/coerce-intvar?)
                              (s/tuple #{'offset} nat-int?))))))

(compile-function compiler constraint-name [*conformed *model]
 (let [coerce-var (coerce-var *model)]
   (match *conformed
     {:args [:ints [?value [_ [:ints ?vars]] [_ ?index] [_ ?offset]]]}
     (.element *model ?value (int-array ?vars) (coerce-var ?index) ?offset)

     {:args [:ints [?value [_ [:int-vars ?vars]] [_ ?index] [_ ?offset]]]}
     (.element *model ?value (into-array IntVar ?vars) (coerce-var ?index) ?offset)

     {:args [:sets [?value [_ ?vars] [_ ?index] [_ ?offset]]]}
     (.element *model (coerce-var ?index) (into-array SetVar ?vars) ?offset ?value))))

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
                  table
                 table)]
     (constraint constraint-name
                 [value
                  ['in table]
                  ['at index]
                  ['offset  offset]]
                 compiler))))
