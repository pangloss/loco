(ns loco.constraints.nth
  (:use loco.constraints.utils)
  (:require
   [loco.match :refer [match+]]
   [clojure.core.match :refer [match]]))

(defn nth
  "partial for $element"
  {:choco "element(IntVar value, IntVar[] table, IntVar index, int offset)"
   :partial true}
  ([vars index]
   (nth vars index 0))

  ([vars index offset]
   {:pre [(nat-int? offset) (sequential? vars)]}
   (let [table (if (every? int? vars)
                 (preserve-consts (vec vars))
                 vars)]
     (partial-constraint
      [:$nth [table
              [:at index]
              [:offset (preserve-consts offset)]]]))))
