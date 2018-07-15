(in-ns 'loco.constraints)
(ns loco.constraints.all-equal
  (:require
   [loco.constraints.utils :refer []]
   [loco.constraints :refer [$arithm]]
   [clojure.spec.alpha :as s]
   [clojure.core.match :refer [match]]
   [clojure.walk :as walk])
  (:import
   [org.chocosolver.solver.variables IntVar BoolVar SetVar]))

(def ^:private constraint-name 'all-equal)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args (s/spec
                (s/or
                 :ints (s/coll-of int-var?)
                 :sets (s/coll-of set-var?)))))

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:args [:ints vars]}
           (.allEqual model (into-array IntVar vars))

           {:args [:sets vars]}
           (.allEqual model (into-array SetVar vars))

           ::s/invalid
           (report-spec-error constraint-name ::compile-spec var-subed-statement))))

(defn $all-equal
  "Constrains that all vars are equal to each other

  Creates a constraint stating that ints should be all equal.
  Creates a constraint stating that sets should be all equal."
  {:choco ["allEqual(IntVar... vars)"
           "allEqual(SetVar... sets)"]}
  [vars]
  {:pre [(vector? vars)]}
  (constraint constraint-name (vec vars)
              compiler))

;;TODO: partial implementation
(defn $=
  [& more]
  (let [morev (vec more)]
    (match [morev]
           [[x y]] ($arithm x = y)
           [_]       ($all-equal morev))))

(reset-meta! (var $=) (meta (var $all-equal)))
