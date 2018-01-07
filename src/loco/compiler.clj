(ns loco.compiler
  (:use loco.constraints
        loco.utils)
  (:require
   [clojure.core.match :refer [match]]
   )
  (:import org.chocosolver.solver.Model)
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

(defn compile-constraint-statement [model statement]
  (->
   [statement]
   ))

(defn compile
  ([ast] (compile (Model.) ast))
  ([model ast]
   (let [vars-index (->>
                     ast
                     (map (juxt second (partial compile-var-statement model)))
                     (into {}))

         ])
   (->>
    ast
    (map (juxt second (partial compile-var-statement model)))
    (into {})
    (list model)
    )))
