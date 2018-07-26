(ns loco.constraints.all-equal
  (:require
   [clojure.core.match :refer [match]]
   [clojure.spec.alpha :as s]
   [clojure.walk :as walk]
   [loco.constraints.arithm :refer [$arithm]]
   [loco.utils :refer [p]]
   [loco.constraints.utils :refer :all :as utils]
   )
  (:import
   [org.chocosolver.solver.variables IntVar BoolVar SetVar]))

(def ^:private constraint-name 'all-equal)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args (s/spec
                (s/or
                 :ints ::utils/coll-coerce-intvar?
                 :sets ::utils/coll-setvar?))))

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))
        coerce-int-var (p utils/coerce-int-var model)]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:args [:ints vars]}
           (.allEqual model (->> vars (map coerce-int-var) (into-array IntVar)))

           {:args [:sets vars]}
           (.allEqual model (into-array SetVar vars))

           ::s/invalid
           (report-spec-error constraint-name ::compile-spec var-subed-statement))))

(defloco $all-equal
  "Constrains that all vars are equal to each other

  Creates a constraint stating that ints should be all equal.
  Creates a constraint stating that sets should be all equal."
  {:choco ["allEqual(IntVar... vars)"
           "allEqual(SetVar... sets)"]}
  [vars]
  {:pre [(vector? vars)]}
  (constraint constraint-name (vec vars)
              compiler))

(defloco $=
  [& more]
  (let [morev (vec more)]
    (match morev
           [x y]   ($arithm x = y)
           [[x y]] ($arithm x = y)
           [(v :guard vector?)] ($all-equal v)
           _     ($all-equal morev))))

(reset-meta! (var $=) (meta (var $all-equal)))
