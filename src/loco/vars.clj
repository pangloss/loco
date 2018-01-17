(ns loco.vars
  (:require [clojure.core.match :refer [match]]
            [clojure.set :as set]))

;;TODO: var methods:
;;boolVar, boolVar, boolVar, boolVar,
;;boolVarArray, boolVarArray,
;;boolVarMatrix, boolVarMatrix,
;;checkIntDomainRange, checkRealDomainRange,
;;generateName, generateName,
;;intVar, intVar, intVar, intVar, intVar, intVar, intVar, intVar,
;;intVarArray, intVarArray, intVarArray, intVarArray, intVarArray, intVarArray,
;;intVarMatrix, intVarMatrix, intVarMatrix, intVarMatrix, intVarMatrix, intVarMatrix,
;;realVar, realVar, realVar, realVar, realVar,
;;realVarArray, realVarArray, realVarMatrix, realVarMatrix,
;;setVar, setVar, setVar, setVar,
;;setVarArray, setVarArray,
;;setVarMatrix, setVarMatrix,
;;taskVar, taskVar, taskVar,
;;taskVarArray,
;;taskVarMatrix,
;;toBoolVar

;; -------------------- Sets --------------------

;; default SetVar[] 	setVarArray(int size, int[] lb, int[] ub)
;; Creates an array of size set variables, taking their domain in [lb, ub]
;; default SetVar[] 	setVarArray(String name, int size, int[] lb, int[] ub)
;; Creates an array of size set variables, taking their domain in [lb, ub]
;; default SetVar[][] 	setVarMatrix(int dim1, int dim2, int[] lb, int[] ub)
;; Creates a matrix of dim1*dim2 set variables, taking their domain in [lb, ub]
;; default SetVar[][] 	setVarMatrix(String name, int dim1, int dim2, int[] lb, int[] ub)
;; Creates a matrix of dim1*dim2 set variables, taking their domain in [lb, ub]

(defn set
  "Creates a set variable taking its domain in [lb, ub], For
   instance [#{0,3}, #{-2,0,2,3}] means the variable must include both
   0 and 3 and can additionnaly include -2, and 2

  if only a single int set is given, then Creates a constant set variable"
  {:choco ["setVar(String name, int[] lb, int[] ub)"
           "setVar(String name, int... value)"]}
  ([var-name lb ub]
   ;;TODO: possible that lb should be subset of ub
   {:pre [(or (set? lb) (sequential? lb)) (or (set? ub) (sequential? ub))
          ;;all elements from lb must be in ub
          (every? (clojure.core/set ub) (clojure.core/set lb))
          (every? integer? lb)
          (every? integer? ub)]}

   [:var var-name :public [:set (clojure.core/set lb) (clojure.core/set ub)]])

  ([var-name ints]
   {:pre [(or (set? ints) (sequential? ints))
          (every? integer? ints)]}

   [:var var-name :public [:set (clojure.core/set ints)]]))

(def set- (comp #(assoc % 2 :hidden) (partial set)))
(reset-meta! (var set-) (meta (var set)))
;; -------------------- Utils --------------------

(defn- hidden-name? [keyword-name]
  (.startsWith (name keyword-name) "_"))

(defn- hidden-conversion
  "this is for backwards compatibility"
  [var]
  (match var
         [_  [(name :guard #(and (keyword? %) (hidden-name? %))) & _] & _] (assoc var 2 :hidden)
         [_  (name :guard #(and (keyword? %) (hidden-name? %))) & _] (assoc var 2 :hidden)
         :else var))

;; -------------------- Constants --------------------

(defn const
  "Declares that a variable must be a specific value (integer)"
  [var-name value]
  {:pre [(integer? value)]}
  (->> [:var var-name :public [:const value]]))

(def const- (comp #(assoc % 2 :hidden) (partial const)))
(reset-meta! (var const-) (meta (var const)))

;; -------------------- booleans --------------------

(defn bool
  "Declares that a variable must be a boolean (true/false or [0 1])
  some constraints have optimizations for booleans/boolean-lists (e.g. Model.sum|and|or)"
  [var-name]
  (->> [:var var-name :public [:bool 0 1]]
       hidden-conversion))

(def bool- (comp #(assoc % 2 :hidden) (partial bool)))
(reset-meta! (var bool-) (meta (var bool)))

(defn bools
  "Declares a list of booleans with given var-names"
  [& var-names]
  (->
   (mapv bool var-names)
   (with-meta {:generated-vars true})))

;; -------------------- Integers --------------------

(defn int
  "Declares that a variable must be in a certain domain.
   Possible arglist examples:
   (in :x 1)
   (in :x 1 5)
   (in :x [1 2 3 4 5])
   (in :x 1 5 :bounded)"
  ([var-name lb ub bounded?]
   {:pre [(integer? lb) (integer? ub) (or (boolean? bounded?) (= bounded? :bounded))]}
   (->>
    (if bounded?
      [:var var-name :public [:int lb ub :bounded]]
      (int var-name lb ub))
    hidden-conversion))

  ([var-name lb ub]
   (->>
    (match (sort [lb ub])
           [0 1] (bool var-name)
           :else [:var var-name :public [:int lb ub]])
    hidden-conversion))

  ([var-name values]
   {:pre [(or
           (integer? values)
           (and (sequential? values)
                (every? integer? values)
                (apply <= values))
           )]}
   (->>
    (match (vec (dedupe (flatten [values])))
           [single-value-domain] (const var-name single-value-domain)
           [0 1] (bool var-name)
           domain-list [:var var-name :public [:int domain-list]])
    hidden-conversion)))

(def int- (comp #(assoc % 2 :hidden) (partial int)))

(def in int)
(def in- int-)

(reset-meta! (var in) (meta (var int)))
(reset-meta! (var in-) (meta (var int)))
(reset-meta! (var int-) (meta (var int)))
