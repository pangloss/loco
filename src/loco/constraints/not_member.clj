(ns loco.constraints.not-member
  (:refer-clojure :exclude [set])

  (:require
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :refer :all :as utils]

   [meander.epsilon :as m :refer [match]]
   [clojure.walk :as walk])
  (:import
   [org.chocosolver.solver.variables SetVar IntVar BoolVar Task]))

(def ^:private constraint-name 'not-member)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/or
                       :int-table (s/tuple
                                   int-var? #{'of} (s/coll-of int?))
                       :int-lb-ub (s/tuple
                                   int-var?
                                   (s/tuple #{'lb} int?)
                                   (s/tuple #{'ub} int?))

                       :int-set   (s/tuple
                                   int-or-intvar? #{'of} set-var?)))))

(compile-function compiler constraint-name [*conformed *model]
 (match *conformed
   {:args [:int-lb-ub [?not-member [_ ?lb] [_ ?ub]]]}
   (.notMember *model ?not-member ?lb ?ub)

   {:args [:int-set [?not-member _ ?set-var]]}
   (.notMember *model ?not-member ?set-var)

   {:args [:int-table [?not-member _ ?table]]}
   (.notMember *model ?not-member (int-array ?table))))

(defn $not-member
  "-------------------- IntVar --------------------
  Creates a member constraint. Ensures var does not take its values in [LB, UB]
  Creates a member constraint. Ensures var does not take its values in table

  -------------------- SetVar --------------------
  Creates a member constraint stating that the constant cst is not in set
  Creates a member constraint stating that the value of intVar is not in set"
  {:choco ["notMember(IntVar var, int[] table)"
           "notMember(IntVar var, int lb, int ub)"
           "notMember(int cst, SetVar set)"
           "notMember(IntVar var, SetVar set)"]}
  ([not-member-of collection]
   (match [not-member-of collection]
          [?not-member (m/pred sequential? ?table)]
          (constraint constraint-name
                      [?not-member 'of  (vec ?table)]
                      compiler)

          ;;TODO: opprotunity to generate vars (set)
          [?not-member (m/pred keyword? ?set)]
          (constraint constraint-name
                      [?not-member 'of ?set]
                      compiler)))

  ([not-member lb ub]
   {:pre [(int? lb) (int? ub) (< lb ub)]}
   (constraint constraint-name
               [not-member
                ['lb  lb]
                ['ub  ub]]
               compiler)))
