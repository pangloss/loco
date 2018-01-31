(ns loco.constraints.member
  (:refer-clojure :exclude [set])
  (:use loco.constraints.utils)
  (:require
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :as utils]
   [loco.match :refer [match+]]
   [clojure.core.match :refer [match]]
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

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:args [:int-lb-ub [member [_ lb] [_ ub]]]}
           (.member model member lb ub)

           {:args [:int-set [member _ set-var]]}
           (.member model member set-var)

           {:args [:set-sets [member _ sets]]}
           (.member model (into-array SetVar sets) member)

           {:args [:int-table [member _ table]]}
           (.member model member (int-array table))

           ::s/invalid
           (report-spec-error constraint-name ::compile-spec var-subed-statement))))

(defn member
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
          [member (table :guard sequential?)]
          (constraint constraint-name
                      [member 'of (preserve-consts (vec table))]
                      compiler)

          ;;TODO: opprotunity to generate vars (set)
          [member (set :guard keyword?)]
          (constraint constraint-name
                      [(preserve-consts member) 'of set]
                      compiler)))

  ([var lb ub]
   {:pre [(int? lb) (int? ub) (< lb ub)]}
   (constraint constraint-name
               [var
                ['lb (preserve-consts lb)]
                ['ub (preserve-consts ub)]]
               compiler)))
