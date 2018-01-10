(ns loco.compiler
  (:use loco.constraints
        loco.utils)
  (:require
   [loco.model :as model]
   [clojure.core.match :refer [match]]
   )
  (:import org.chocosolver.solver.Model
           org.chocosolver.solver.variables.BoolVar
           org.chocosolver.solver.variables.IntVar)
  )

(defn- lookup-var [vars-index name]
  (if-let [var (get vars-index name)]
    var
    (if (number? name)
      name
      (throw (Exception. "Could not find variable: " name)))))

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
             )]
    [(assoc vars-index (second statement) var)
     (conj vars var)
     model]))

;;handles boolVars and intVars
(defn- sum-constraint [model sum-vars op eq-var]
  (if-let [homogeneous? (->> sum-vars (map class) (apply =))]
    (.sum model (into-array sum-vars) (name op) eq-var)
    (let [casted-to-intvars (map #(cast IntVar %) sum-vars)]
      (.sum model
            (into-array IntVar casted-to-intvars)
            (name op)
            eq-var))))

(defn compile-constraint-statement [vars-index model statement]
  (let [lookup-var (partial lookup-var vars-index)]
    (->
     statement
     (match [:constraint constraint] constraint)
     (match
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

      ;;TODO: there is optimization for bools on min and max
      [:min [result :of vars]]
      (.min model (lookup-var result) (->> vars (map lookup-var) (into-array IntVar)))

      [:max [result :of vars]]
      (.max model (lookup-var result) (->> vars (map lookup-var) (into-array IntVar)))

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

      [:square [result dep]]
      (.square model (lookup-var result) (lookup-var dep))
      ))))

(defn compile-vars [model ast]
  (->>
   ast
   (reduce compile-var-statement [{} [] model])))

(defn compile-constraints [model vars-index ast]
  (->>
   ast
   (map (partial compile-constraint-statement vars-index model))))

;;FIXME: there is going to be an issue with reify, as it is part of a constraint object
(defn compile
  ([ast] (compile (Model.) ast))
  ([model ast]
   (let [
         uncompiled-vars (->> ast (filter model/var?))
         uncompiled-constraints (->> ast (filter model/constraint?))
         [vars-index vars _] (compile-vars model uncompiled-vars)
         ;;vars-index (zipmap (map second uncompiled-vars) vars)
         constraints (->>
                      (compile-constraints model vars-index uncompiled-constraints)
                      (map (juxt identity (memfn post)))
                      (map first)
                      doall)
         ]
     {:vars-index vars-index
      :vars vars
      :constraints constraints
      :model model})))
