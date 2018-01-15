(ns loco.vars
  (:require [clojure.core.match :refer [match]]))

(defn- hidden-name? [keyword-name]
  (.startsWith (name keyword-name) "_"))

(defn- hidden-conversion
  "this is for backwards compatibility"
  [var]
  (match var
         [_  [(name :guard #(and (keyword? %) (hidden-name? %))) & _] & _] (assoc var 2 :hidden)
         [_  (name :guard #(and (keyword? %) (hidden-name? %))) & _] (assoc var 2 :hidden)
         :else var))

(defn const
  "Declares that a variable must be a specific value (integer)"
  [var-name value]
  {:pre [(integer? value)]}
  (->> [:var var-name :public [:const value]]))

(def const- (comp #(assoc % 2 :hidden) (partial const)))
(reset-meta! (var const-) (meta (var const)))

(defn bool
  "Declares that a variable must be a boolean (true/false or [0 1])
  some constraints have optimizations for booleans/boolean-lists (e.g. Model.sum|and|or)"
  [var-name]
  (->> [:var var-name :public [:bool 0 1]]
       hidden-conversion))

(def bool- (comp #(assoc % 2 :hidden) (partial bool)))
(reset-meta! (var bool-) (meta (var bool)))

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
