(ns loco.constraints.member
  (:refer-clojure :exclude [set])

  (:require
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :refer :all :as utils]

   [meander.epsilon :as m :refer [match]]
   [clojure.walk :as walk])
  (:import
   [org.chocosolver.solver.variables SetVar IntVar BoolVar Task]))

(def ^:private constraint-name 'member)

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
                                   int-or-intvar? #{'of} set-var?)

                       :set-sets  (s/tuple set-var? #{'of} (s/coll-of set-var?))))))

(compile-function compiler constraint-name [*conformed *model]
 (match *conformed
   {:args [:int-lb-ub [?member [_ ?lb] [_ ?ub]]]}
   (.member *model ?member ?lb ?ub)

   {:args [:int-set [?member _ ?set-var]]}
   (.member *model ?member ?set-var)

   {:args [:set-sets [?member _ ?sets]]}
   (.member *model (into-array SetVar ?sets) ?member)

   {:args [:int-table [?member _ ?table]]}
   (.member *model ?member (int-array ?table))))

(defn $member
  "-------------------- IntVar --------------------
  Creates a member constraint. Ensures var takes its values in [LB, UB]
  Creates a member constraint. Ensures var takes its values in table

  -------------------- SetVar --------------------
  Creates a member constraint stating that the constant cst is in set
  Creates a member constraint stating that the value of intVar is in set
  Creates a member constraint stating that set belongs to sets"
  {:choco ["member(IntVar var, int[] table)"
           "member(IntVar var, int lb, int ub)"
           "member(int cst, SetVar set)"
           "member(IntVar intVar, SetVar set)"
           "member(SetVar[] sets, SetVar set)"]}
  ([member-of collection]
   (match [member-of collection]
     [?member (m/pred sequential? ?table)]
     (constraint constraint-name
                 [?member 'of  (vec ?table)]
                 compiler)

     ;;TODO: opprotunity to generate vars (set)
     [?member (m/pred keyword? ?set)]
     (constraint constraint-name
                 [?member 'of ?set]
                 compiler)))

  ([var lb ub]
   {:pre [(int? lb) (int? ub) (< lb ub)]}
   (constraint constraint-name
               [var
                ['lb  lb]
                ['ub  ub]]
               compiler)))
