(ns loco.constraints.not-member
  (:use loco.constraints.utils)
  (:require
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :as utils]
   [loco.match :refer [match+]]
   [clojure.core.match :refer [match]]
   [clojure.walk :as walk])
  (:import
   [org.chocosolver.solver.variables SetVar IntVar BoolVar Task]))

(def ^:private constraint-name 'not-member)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/or
                       :int-table (s/tuple int-var? #{'of} (s/coll-of int?))
                       :int-lb-ub (s/tuple
                                   int-var?
                                   (s/tuple #{'lb} int?)
                                   (s/tuple #{'ub} int?))

                       :int-set   (s/tuple
                                   int-or-intvar?
                                   (s/tuple #{'of} set-var?))))))

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:args [:int-lb-ub [not-member [_ lb] [_ ub]]]}
           (.notMember model not-member lb ub)

           {:args [:int-set [not-member _ set-var]]}
           (.notMember model not-member set-var)

           [:not-member [:int-table [not-member _ table]]]
           (.notMember model not-member (int-array table))

           ::s/invalid
           (report-spec-error constraint-name ::compile-spec var-subed-statement))))

(defn not-member
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
          [not-member (table :guard sequential?)]
          (constraint constraint-name
                      [not-member 'of (preserve-consts (vec table))]
                      compiler)

          ;;TODO: opprotunity to generate vars (set)
          [not-member (set :guard keyword?)]
          (constraint constraint-name
                      [(preserve-consts not-member) 'of set]
                      compiler)))

  ([not-member lb ub]
   {:pre [(int? lb) (int? ub) (< lb ub)]}
   (constraint constraint-name
               [not-member
                ['lb (preserve-consts lb)]
                ['ub (preserve-consts ub)]]
               compiler)))
