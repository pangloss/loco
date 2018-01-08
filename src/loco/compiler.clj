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

(defn compile-var-statement [model statement]
  (->
   statement
   (match
    [:var var-name _ [:bool _ _]]
    (.boolVar model (name var-name))

    [:var var-name _ [:int (lb :guard integer?) (ub :guard integer?)]]
    (.intVar model (name var-name) lb ub)

    [:var var-name _ [:int (lb :guard integer?) (ub :guard integer?) :bounded]]
    (.intVar model (name var-name) lb ub true)

    [:var var-name _ [:const (value :guard integer?)]]
    (.intVar model (name var-name) value)

    [:var var-name _ [:int (enumeration :guard vector?)]]
    (.intVar model (name var-name) (int-array enumeration))
    )))

;;handles boolVars and intVars
(defn- post-sum-constraint [model sum-vars op eq-var]
  (let [homogeneous? (->> sum-vars (map class) (apply =))]
    (.post
     (if homogeneous?
       (.sum model (into-array sum-vars) (name op) eq-var)
       (let [casted-to-intvars (mapv #(cast IntVar %) sum-vars)]
         (.sum model
               (into-array IntVar casted-to-intvars)
               (name op)
               eq-var))))))

(defn compile-constraint-statement [vars-index model statement]
  (let [lookup-var (partial get vars-index)]
    (->
     statement
     (match
      [:constraint [:sum [eq-var op (sum-vars :guard vector?)]]]
      (post-sum-constraint model (map lookup-var sum-vars) (name op) (lookup-var eq-var))
      ))))

(defn compile-vars [model ast]
  (->>
   ast
   (filter model/var?)
   (map (juxt second (partial compile-var-statement model)))
   (into {})))

(defn compile-constraints [model vars-index ast]
  (->>
   ast
   (filter model/constraint?)
   (map (partial compile-constraint-statement vars-index model))))

(defn compile
  ([ast] (compile (Model.) ast))
  ([model ast]
   (let [vars-index (compile-vars model ast)
         constraints (doall (compile-constraints model vars-index ast))
         ]
     {:vars vars-index
      :constraints constraints
      :model model})))
