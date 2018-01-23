(ns loco.constraints.sum
  (:use loco.utils
        loco.constraints.utils)
  (:require [clojure.core.match :refer [match]]
            [loco.match :refer [match+]])
  (:import
   org.chocosolver.solver.variables.IntVar
   org.chocosolver.solver.variables.BoolVar
   org.chocosolver.solver.variables.SetVar))

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

(defn- sum-constraint
  "this handles the IntVar and BoolVar arguments method dispatching for Model.sum"
  [model sum-vars op eq-var]
  (cond
    (every? (p instance? BoolVar) sum-vars)
    (.sum model
          (into-array BoolVar (map (p cast BoolVar) sum-vars))
          (name op)
          eq-var)

    :else (.sum model
                (into-array IntVar (map (p cast IntVar) sum-vars))
                (name op)
                eq-var)))

(defn- sum-compiler [model vars-index statement]
  (let [
        lookup-var (partial lookup-var vars-index)
        lookup-var-unchecked (partial lookup-var-unchecked vars-index)
        int-var? (p instance? IntVar)
        set-var? (p instance? SetVar)
        lookup-set-var? (c set-var? lookup-var-unchecked)
        lookup-int-var? (c int-var? lookup-var-unchecked)
        all-lookup-int-vars? (p every? lookup-int-var?)
        all-lookup-set-vars? (p every? lookup-set-var?)
        all-int-vars? (p every? int-var?)
        all-set-vars? (p every? set-var?)
        ]
    (match+ (clojure.walk/prewalk-replace vars-index statement)
            [_:sum [eq-var op [& sum-vars]]]
            :guard [op #{:=}]
            (sum-constraint model sum-vars op eq-var)

            [_:sum [eq-var := set-var]] :guard [set-var set-var?]
            (.sum model set-var eq-var))))

(defn sum
  "Creates a sum constraint. Enforces that ∑i in |vars|varsi operator sum
  Creates a constraint summing elements of set sum{i | i in set} = sum"
  {:choco ["sum(IntVar[] vars, String operator, IntVar sum)"
           "sum(SetVar set, IntVar sum)"]
   :partial true}
  ([vars]
   {:pre [(sequential? vars)]}
   [:constraint :partial [:+ vars]]) ;; this is named differently
                                       ;; because it creates nice var
                                       ;; names. gets converted into a
                                       ;; :sum at compile step

  ([summation set-var]
   (-> [:constraint [:sum [summation := set-var]]]
       (with-meta {:compiler (var sum-compiler)})))

  ([summation operator vars]
   {:pre [(sequential? vars) (comparison-operator? operator)]}
   (-> [:constraint [:sum [summation operator vars]]]
       (with-meta {:compiler (var sum-compiler)}))))
