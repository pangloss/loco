(ns loco.compiler
  (:use loco.constraints
        loco.utils)
  (:require
   [loco.model :as model]
   [clojure.core.match :refer [match]])
  (:import org.chocosolver.solver.Model
           org.chocosolver.solver.variables.SetVar
           org.chocosolver.solver.variables.BoolVar
           org.chocosolver.solver.variables.IntVar
           org.chocosolver.solver.constraints.Constraint))

(defn- lookup-var [vars-index name]
  (if-let [var (get vars-index name)]
    var
    (if (number? name)
      name
      (throw (Exception. (str "Could not find variable: " name))))))

(defn- lookup-var-unchecked [vars-index name]
  (if-let [var (get vars-index name)]
    var
    (when (number? name)
      name)))

(defn compile-var-statement [[vars-index vars model] statement]
  (let [var (match
             [statement (meta statement)]

             [[:var var-name _ _] {:neg dep-name}]
             (.intMinusView model (lookup-var vars-index dep-name))

             [[:var var-name _ [:bool _ _]] _]
             (.boolVar model (name var-name))

             [[:var var-name _ [:int (lb :guard integer?) (ub :guard integer?)]] _]
             (.intVar model (name var-name) lb ub)

             [[:var var-name _ [:int (lb :guard integer?) (ub :guard integer?) :bounded]] _]
             (.intVar model (name var-name) lb ub true)

             [[:var var-name _ [:const (value :guard integer?)]] _]
             (.intVar model (name var-name) value)

             [[:var var-name _ [:int (enumeration :guard vector?)]] _]
             (.intVar model (name var-name) (int-array enumeration))

             [[:var var-name _ [:set (constants :guard set?)]] _]
             (.setVar model (name var-name) (into-array Integer/TYPE constants))

             [[:var var-name _ [:set (lb :guard set?) (ub :guard set?)]] _]
             (.setVar model (name var-name)
                      (into-array Integer/TYPE lb)
                      (into-array Integer/TYPE ub))
             )]
    [(-> vars-index
         (with-meta {:ast-statement statement})
         (assoc (second statement) var))
     (conj vars var)
     model]))

;;TODO: refactor these boolean optimizations, they are all the same
(defn- sum-constraint
  "this handles the IntVar and BoolVar arguments method dispatching for Model.sum"
  [model sum-vars op eq-var]
  (if-let [homogeneous? (->> sum-vars (map class) (apply =))]
    (.sum model (into-array sum-vars) (name op) eq-var)
    (let [casted-to-intvars (map #(cast IntVar %) sum-vars)]
      (.sum model
            (into-array IntVar casted-to-intvars)
            (name op)
            eq-var))))

(defn- min-constraint
  "this handles the IntVar and BoolVar arguments method dispatching for Model.min"
  [model eq-var min-vars]
  (if-let [homogeneous? (->> min-vars (map class) (apply =))]
    (.min model eq-var (into-array min-vars))
    (let [casted-to-intvars (map #(cast IntVar %) min-vars)]
      (.min model eq-var (into-array IntVar casted-to-intvars)))))

(defn- max-constraint
  "this handles the IntVar and BoolVar arguments method dispatching for Model.max"
  [model eq-var max-vars]
  (if-let [homogeneous? (->> max-vars (map class) (apply =))]
    (.max model eq-var (into-array max-vars))
    (let [casted-to-intvars (map #(cast IntVar %) max-vars)]
      (.max model eq-var (into-array IntVar casted-to-intvars)))))

(defn compile-constraint-statement [vars-index model statement]
  (let [lookup-var (partial lookup-var vars-index)
        lookup-var-unchecked (partial lookup-var-unchecked vars-index)
        realize-nested-constraints (fn [constraints]
                                     (->> constraints
                                          (map (p compile-constraint-statement vars-index model))
                                          (into-array Constraint)
                                          ))
        realize-nested-constraint (p compile-constraint-statement vars-index model)
        int-var? (p instance? IntVar)
        set-var? (p instance? SetVar)
        lookup-set-var? (c set-var? lookup-var-unchecked)
        ]
    (->
     statement
     (match [:constraint constraint] constraint)
     (match
      [:sum [(set-var :guard lookup-set-var?) := eq-var]]
      (.sum model (lookup-var set-var) (lookup-var eq-var))

      [:sum [eq-var := (set-var :guard lookup-set-var?)]]
      (.sum model (lookup-var set-var) (lookup-var eq-var))

      [:sum [eq-var op (sum-vars :guard vector?)]]
      (sum-constraint model (map lookup-var sum-vars) (name op) (lookup-var eq-var))

      [:arithm [comp-var comp-op var1 op var2]]
      (.arithm model
               (lookup-var comp-var)
               (name comp-op)
               (lookup-var var1)
               (name op)
               (lookup-var var2))

      [:arithm [comp-var comp-op var]]
      (.arithm model
               (lookup-var comp-var)
               (name comp-op)
               (lookup-var var))

      [:times [comp-var := var1 :* var2]]
      (.times model
               (lookup-var var1)
               (lookup-var var2)
               (lookup-var comp-var))

      [:mod [comp-var := var1 :% var2]]
      (.mod model
            (lookup-var var1)
            (lookup-var var2)
            (lookup-var comp-var))

      [:abs [abs-var := var]]
      (.absolute model (lookup-var abs-var) (lookup-var var))

      [:div [result := numerator :/ denominator ]]
      (.div model (lookup-var numerator) (lookup-var denominator) (lookup-var result))

      [:all-equal vars]
      (.allEqual model (->> vars (map lookup-var) (into-array IntVar)))

      [:not-all-equal vars]
      (.notAllEqual model (->> vars (map lookup-var) (into-array IntVar)))

      [:min [min
             [:of weights]
             [:indices set-indices]
             [:offset offset]
             [:not-empty? not-empty?]]]
      (.min model
            (lookup-var set-indices)
            (into-array Integer/TYPE weights)
            offset
            (lookup-var min)
            not-empty?)

      [:min [min [:of set-var] [:not-empty? not-empty?]]]
      (.min model (lookup-var set-var) (lookup-var min) not-empty?)

      [:min [min [:of vars]]]
      (min-constraint model (lookup-var min) (map lookup-var vars))

      [:max [max
             [:of weights]
             [:indices set-indices]
             [:offset offset]
             [:not-empty? not-empty?]]]
      (.max model
            (lookup-var set-indices)
            (into-array Integer/TYPE weights)
            offset
            (lookup-var max)
            not-empty?)

      [:max [max [:of set-var] [:not-empty? not-empty?]]]
      (.max model (lookup-var set-var) (lookup-var max) not-empty?)

      [:max [max [:of vars]]]
      (max-constraint model (lookup-var max) (map lookup-var vars))

      [:scalar [result op vars coeffs]]
      (.scalar model
               (->> vars (map lookup-var) (into-array IntVar))
               (int-array coeffs)
               (name op)
               (lookup-var result))

      [:element [result [:in (vars :guard #(every? integer? %))] [:at index] [:offset offset]]]
      (.element model
                (lookup-var result)
                (int-array vars)
                (lookup-var index)
                offset)

      [:element [result [:in (vars :guard #(every? keyword? %))] [:at index] [:offset offset]]]
      (.element model
                (lookup-var result)
                (->> vars (map lookup-var) (into-array IntVar))
                (lookup-var index)
                offset)

      [:distinct vars]
      (.allDifferent model (->> vars (map lookup-var) (into-array IntVar)) "DEFAULT")

      [:distinct-except-0 vars]
      (.allDifferentExcept0 model (->> vars (map lookup-var) (into-array IntVar)))

      [:circuit [vars [:offset offset]]]
      (.circuit model (->> vars (map lookup-var) (into-array IntVar)) offset)

      [:cardinality [vars [values occurrences] [:closed closed?]]]
      (.globalCardinality model
                          (->> vars (map lookup-var) (into-array IntVar))
                          (int-array values)
                          (->> occurrences (map lookup-var) (into-array IntVar))
                          closed?)

      [:knapsack [[:weight weights] [:energy energies]
                  [:occurrences occurrences] [:weight-sum weight-sum] [:energy-sum energy-sum]]]
      (.knapsack model
                 (->> occurrences (map lookup-var) (into-array IntVar))
                 (lookup-var weight-sum)
                 (lookup-var energy-sum)
                 (int-array weights)
                 (int-array energies))

      [:regular [vars [:automation automation]]]
      (.regular model
                (->> vars (map lookup-var) (into-array IntVar))
                automation)

      [:square [result dep]]
      (.square model (lookup-var result) (lookup-var dep))

      [:member [var [:lower-bound lb] [:upper-bound ub]]]
      (.member model (lookup-var var) lb ub)

      [:member [var :of (set-var :guard lookup-set-var?)]]
      (.member model (lookup-var var) (lookup-var set-var))

      [:member [var :of (table :guard (p every? integer?))]]
      (.member model (lookup-var var) (int-array table))

      [:not-member [var [:lower-bound lb] [:upper-bound ub]]]
      (.notMember model (lookup-var var) lb ub)

      [:not-member [var :of (set-var :guard lookup-set-var?)]]
      (.notMember model (lookup-var var) (lookup-var set-var))

      [:not-member [var :of (table :guard (p every? integer?))]]
      (.notMember model (lookup-var var) (int-array table))

      [:n-values [vars n-values]]
      (.nValues model (->> vars (map lookup-var) (into-array IntVar)) (lookup-var n-values))

      [:sort [vars sorted-vars]]
      (.sort model
             (->> vars (map lookup-var) (into-array IntVar))
             (->> sorted-vars (map lookup-var) (into-array IntVar)))

      [:count [vars [:value value] [:limit limit]]]
      (.count model
              (lookup-var value)
              (->> vars (map lookup-var) (into-array IntVar))
              (lookup-var limit))

      [:among [vars [:nb-var nb-var] [:values values]]]
      (.among model
              (lookup-var nb-var)
              (->> vars (map lookup-var) (into-array IntVar))
              (int-array values))

      [:at-least-n-values [vars [:n-values n-values] [:ac ac]]]
      (.atLeastNValues model
                       (->> vars (map lookup-var) (into-array IntVar))
                       (lookup-var n-values)
                       ac)

      [:at-most-n-values [vars [:n-values n-values] [:strong strong]]]
      (.atMostNValues model
                      (->> vars (map lookup-var) (into-array IntVar))
                      (lookup-var n-values)
                      strong)

      [:bin-packing
       [[:item-bin item-bin]
        [:item-size item-size]
        [:bin-load bin-load]
        [:offset offset]]]
      (.binPacking model
                   (->> item-bin (map lookup-var) (into-array IntVar))
                   (->> item-size int-array)
                   (->> bin-load (map lookup-var) (into-array IntVar))
                   offset)

      [:bit-channeling [bits int-var]]
      (.bitsIntChanneling model
                          (->> bits (map lookup-var) (into-array BoolVar))
                          (lookup-var int-var))

      ;; handle boolean lists
      [:and (bools :guard (p every? (c (p instance? BoolVar) lookup-var-unchecked)))]
      (.and model (->> bools (map lookup-var) (into-array BoolVar)))

      [:and (constraints :guard (p every? model/constraint?))]
      (.and model (realize-nested-constraints constraints))

      ;; handle boolean lists
      [:or (bools :guard (p every? (c (p instance? BoolVar) lookup-var-unchecked)))]
      (.or model (->> bools (map lookup-var) (into-array BoolVar)))

      [:or (constraints :guard (p every? model/constraint?))]
      (.or model (realize-nested-constraints constraints))

      [:when [(bool :guard (c (p instance? BoolVar) lookup-var-unchecked)) then-constraint]]
      (.ifThen model
               (lookup-var bool)
               (realize-nested-constraint then-constraint))

      [:when [if-constraint then-constraint]]
      (.ifThen model
               (realize-nested-constraint if-constraint)
               (realize-nested-constraint then-constraint))

      [:if-else [(bool :guard (c (p instance? BoolVar) lookup-var-unchecked))
                 then-constraint else-constraint]]
      (.ifThenElse model
                   (lookup-var bool)
                   (realize-nested-constraint then-constraint)
                   (realize-nested-constraint else-constraint))

      [:if-else [if-constraint then-constraint else-constraint]]
      (.ifThenElse model
                   (realize-nested-constraint if-constraint)
                   (realize-nested-constraint then-constraint)
                   (realize-nested-constraint else-constraint))

      [:iff [if-constraint then-constraint]]
      (.ifOnlyIf model
                 (realize-nested-constraint if-constraint)
                 (realize-nested-constraint then-constraint))

      [:not constraint]
      (.not model (realize-nested-constraint constraint))

      :true
      (.trueConstraint model)

      :false
      (.falseConstraint model)
      ))))

(defn compile-reify-statement [vars-index model statement]
  (match statement
         [:reify (var-name :guard (c (p instance? BoolVar)
                                     (p lookup-var-unchecked vars-index))) constraint]
         (.reification model
                       (lookup-var vars-index var-name)
                       (compile-constraint-statement vars-index model constraint))))

(defn compile-reifies [model vars-index ast]
  (->>
   ast
   (map (partial compile-reify-statement vars-index model))
   doall))

(defn compile-vars [model ast]
  (->>
   ast
   (reduce compile-var-statement [{} [] model])))

(defn compile-constraints [model vars-index ast]
  (->>
   ast
   (map (partial compile-constraint-statement vars-index model))
   doall))

(defn compile
  ([ast] (compile (Model.) ast))
  ([model ast]
   (let [
         uncompiled-vars (->> ast (filter model/var?))
         uncompiled-constraints (->> ast (filter model/constraint?))
         uncompiled-reifies (->> ast (filter model/reify?))
         [vars-index vars _] (compile-vars model uncompiled-vars)
         public-var-names (->> uncompiled-vars (filter model/public-var?) (map second))
         public-vars-index (select-keys vars-index public-var-names)
         _reifies (compile-reifies model vars-index uncompiled-reifies)
         constraints (->>
                      (compile-constraints model vars-index uncompiled-constraints)
                      ;;the conditional constraints return void, and are posted automatically
                      ;;the (when %) prevents NULL.post()
                      (map (juxt identity #(when % (.post %))))
                      (map first)
                      doall)]
     {
      :ast ast
      :var-name-mapping (:var-name-mapping (meta ast))
      :constraints constraints
      :model model
      :vars vars
      :vars-map (map vector uncompiled-vars vars)
      :public-vars-index public-vars-index
      :vars-index vars-index
      })))
