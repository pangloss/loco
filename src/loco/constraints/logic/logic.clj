(in-ns 'loco.constraints)
(ns loco.constraints.logic.logic
  (:require
   [loco.constraints.vars :refer [$bool-]]))

(defn $and
  "An \"and\" statement (i.e. \"P^Q^...\"); this statement is true if
  and only if every subconstraint is true."
  {:choco ["and(BoolVar... bools)"
           "and(Constraint... cstrs)"]}
  [& constraints-or-bools]
  {:pre [(sequential? constraints-or-bools) (not (empty? constraints-or-bools))]}
  [:constraint [:and (vec constraints-or-bools)]])

(defn $or
  "An \"or\" statement (i.e. \"PvQv...\"); this statement is true if and
  only if at least one subconstraint is true."
  {:choco ["or(BoolVar... bools)"
           "or(Constraint... cstrs)"]}
  [& constraints-or-bools]
  {:pre [(sequential? constraints-or-bools) (not (empty? constraints-or-bools))]}
  [:constraint [:or (vec constraints-or-bools)]])

(defn $not
  "Given a constraint C, returns \"not C\" a.k.a. \"~C\", which is true iff C is false."
  {:choco "not(Constraint cstr)"}
  [constraint]
  [:constraint [:not constraint]])

(defn $when
  [if-this then-this]
  [:constraint [:when [if-this then-this]]])

(defn $if
  "An \"if\" statement (i.e. \"implies\", \"P=>Q\"); this statement is true if and only if P is false or Q is true.
In other words, if P is true, Q must be true (otherwise the whole
  statement is false).  An optional \"else\" field can be specified,
  which must be true if P is false."
  [if-this then-this else-this]
  [:constraint [:if-else [if-this then-this else-this]]])

(defn $iff
  "Posts an equivalence constraint stating that cstr1 is satisfied <=>
  cstr2 is satisfied, BEWARE : it is automatically posted (it cannot
  be reified)"
  [if-this then-this]
  [:constraint [:iff [if-this then-this]]])

;;TODO: fix up reify to be more like constraint/var format (more meta)
(defn $reify
  "Given a constraint C, will generate a bool-var V such that (V = 1) iff C."
  {:choco "reification(BoolVar var, Constraint cstr)"}
  [var-label, constraint]
  (-> [($bool- var-label)
       ^:reify [:reify var-label constraint]]
      (with-meta {:generated-vars true})))

(defn ^:dynamic *cond-name-gen*
  "useful to change bindings for tests and if you want to use the
  internal bools generated by $cond"
  [prefix] (gensym prefix))

;;this is really complicated... very skeptical of it's use, or even correctly working
(defn $cond
  "A convenience function for constructing a \"cond\"-like statement out of $if/$reify statements.
  The final \"else\" can be specified using the :else keyword.

  the constraints try to behave like a clojure cond would, making sure
  that order is enforced, so each statement requires that all of the
  previous clauses are false.

  uses *cond-name-gen* dynamic function to generate names for the reify variables

  roughly translates to the below code:
  partition clauses by 2
  for each pair, produce:
  --------------------------
  ($reify :if_cond_1 ($false))
  ($reify :if_1 ($and :if_cond_1 ... :not_if_bool_0 ... :not_if_bool_n))
  ($reify :not_if_1 ($not ($false)))

  ($if :if_1
       ($true)
       ($and :not_if_1))
  ---------------------------
  "
  [& clauses]
  {:pre [(even? (count clauses))]}
  (let [global-name (*cond-name-gen* "_")
        last-index (dec (count (partition 2 clauses)))]
    (->>
     (partition 2 clauses)
     (map-indexed vector)
     (reduce
      (fn [acc [idx [clause action]]]
        (let [[previous-not-clauses statements] acc
              if-cond-bool (keyword (str global-name '_if_cond_ idx))
              if-bool (keyword (str global-name '_if_ idx))
              not-if-bool (keyword (str global-name '_not_if_ idx))

              new-statements
              (if (= clause :else)
                [($when (apply $and previous-not-clauses)
                         action)]
                [
                 ($reify if-cond-bool clause)
                 ($reify if-bool
                         (apply $and if-cond-bool previous-not-clauses))
                 ($reify not-if-bool ($not clause)) ;;option to be boolNotView
                 ($if if-bool
                      action
                      ($and not-if-bool))
                 ])
              ]
          [(conj previous-not-clauses not-if-bool), (into statements new-statements)]))
      [[] []])
     second)))

(def $true [:constraint :true])

(def $false [:constraint :false])
