(ns loco.constraints.set
  (:refer-clojure :exclude [not-empty partition])
  (:use loco.utils
        loco.constraints.utils)
  (:require [clojure.core.match :refer [match]]
            [loco.match :refer [match+]]
            [defun.core :refer [defun]]
            loco.automata
            loco.constraints.arithmetic
            loco.vars))

;;TODO: add partials for some of the easier set constraints
(defn intersection
  "Creates a constraint which ensures that the intersection of sets is equal to intersectionSet
  Creates a constraint which ensures that the intersection of sets is equal to intersectionSet"
  {:choco ["intersection(SetVar[] sets, SetVar intersectionSet)"
           "intersection(SetVar[] sets, SetVar intersectionSet, boolean boundConsistent)"]}
  ([intersection-set sets] (intersection intersection-set sets false))

  ([intersection-set sets bounds-consistent?]
   {:pre [(sequential? sets) (boolean? bounds-consistent?)]}
   [:constraint [:intersection [intersection-set
                                [:of (vec sets)]
                                [:bound-consistent bounds-consistent?]]]]))

(defn union
  "Creates a constraint ensuring that union is exactly the union of values taken by ints
  Creates a constraint which ensures that the union of sets is equal to unionSet"
  {:choco ["union(IntVar[] ints, SetVar union)"
           "union(SetVar[] sets, SetVar unionSet)"]}
  [union-set collection]
  {:pre [(sequential? collection)]}
  [:constraint [:union [union-set [:of (vec collection)]]]])

(defn inverse
  "Creates a constraint stating that : x in sets[y-offset1] <=> y in invSets[x-offset2]"
  {:choco ["inverseSet(SetVar[] sets, SetVar[] invSets, int offset1, int offset2)"]}
  ([sets offset-set inverse-sets offset-invsere-set]
   {:pre [(integer? offset-invsere-set)
          (integer? offset-set)
          (sequential? inverse-sets)
          (sequential? sets)]}
   [:constraint [:inverse [[:inverse-sets (vec inverse-sets)
                            :offset (preserve-consts offset-invsere-set)]
                           [:sets (vec sets)
                            :offset (preserve-consts offset-set)]]]]))

(defn nb-empty
  "Creates a constraint counting the number of empty sets sets |{s in sets where |s|=0}| = nbEmpty"
  {:choco ["nbEmpty(SetVar[] sets, int nbEmpty)"
           "nbEmpty(SetVar[] sets, IntVar nbEmpty)"]}
  [num-empty-sets collection]
  {:pre [(sequential? collection)]}
  [:constraint [:nb-empty [(preserve-consts num-empty-sets) [:of (vec collection)]]]])

(def count-empty nb-empty)
(reset-meta! (var count-empty) (meta (var nb-empty)))

(defn not-empty
  "Creates a constraint preventing set to be empty"
  {:choco "notEmpty(SetVar set)"}
  [set-var]
  [:constraint [:not-empty set-var]])

(defn off-set
  "Creates a constraint linking set1 and set2
  with an index offset : x in set1 <=> x+offset in set2"
  {:choco "offSet(SetVar set1, SetVar set2, int offset)"}
  [set1 set2 offset]
  {:pre [(integer? offset)]}
  [:constraint [:off-set [set1 set2 [:offset (preserve-consts offset)]]]])

(defn partition
  "Creates a constraint stating that partitions universe into sets: union(sets) = universe intersection(sets) = {}"
  {:choco "partition(SetVar[] sets, SetVar universe)"}
  [universe collection]
  {:pre [(sequential? collection)]}
  [:constraint [:partition [collection [:universe universe]]]])

(defn subset-equal
  ;; lawl, shit choco docs
  "Creates a constraint establishing that sets[i] is a subset of sets[j] if i"
  {:choco "subsetEq(SetVar... sets)"}
  [& sets]
  (match (vec sets)
         [set-list :guard vector?] [:constraint [:subset-eq (vec set-list)]]
         set-list (subset-equal set-list)))

;;TODO: can do partial for sum-elements
(defn sum-elements
  "Creates a constraint summing weights given by a set of indices:
  sum{weights[i-offset] | i in indices} = sum Also ensures that
  elements in indices belong to [offset, offset+weights.length-1]

  Creates a constraint summing weights given by a set of indices:
  sum{weights[i] | i in indices} = sum Also ensures that elements in
  indices belong to [0, weights.length-1]"
  {:choco ["sumElements(SetVar indices, int[] weights, IntVar sum)"
           "sumElements(SetVar indices, int[] weights, int offset, IntVar sum)"]}
  ([sum-var indices-set weights] (sum-elements sum-var indices-set weights 0))
  ([sum-var indices-set weights offset]
   {:pre [(sequential? weights) (every? integer? weights) (integer? offset)]}
   [:constraint [:sum-elements
                 [sum-var
                  [:indices indices-set]
                  [:weights (preserve-consts (vec weights))]
                  [:offset (preserve-consts offset)]]]]))

(defn symetric
  "Creates a constraint stating that sets are symmetric sets: x in sets[y] <=> y in sets[x]
  Creates a constraint stating that sets are symmetric sets: x in sets[y-offset] <=> y in sets[x-offset]"
  {:choco ["symmetric(SetVar... sets)"
           "symmetric(SetVar[] sets, int offset)"]
   :arglists '([sets]
               [sets offset]
               [& sets])}
  [& sets]
  (match+ (vec sets)
          [set-list offset] :guard [set-list sequential? offset integer?]
          [:constraint [:symetric [(vec set-list) [:offset (preserve-consts offset)]]]]

          [set-list :guard sequential?] (symetric set-list 0)
          set-list (symetric set-list 0)))

(defn all-disjoint
  "Creates a constraint stating that the intersection of sets should be empty Note that there can be multiple empty sets"
  {:choco "allDisjoint(SetVar... sets)"}
  [& sets]
  (match (vec sets)
         [set-list :guard sequential?] [:constraint [:all-disjoint (vec set-list)]]
         set-list (all-disjoint set-list)))

(defn disjoint
  "Creates a constraint stating that the intersection of set1 and set2 should be empty Note that they can be both empty"
  {:choco "disjoint(SetVar set1, SetVar set2)"}
  ([[set1 set2 :as set-pair]]
   {:pre [(= 2 (count set-pair))]}
   (disjoint set1 set2))
  ([set1 set2]
   [:constraint [:disjoint [set1 set2]]]))

(defn set-bools-channeling
  "Creates a constraint channeling a set variable with boolean variables :
  i in set <=> bools[i] = TRUE.

  Creates a constraint channeling a set variable with boolean variables :
  i in set <=> bools[i-offset] = TRUE."
  {:choco ["setBoolsChanneling(BoolVar[] bools, SetVar set)"
           "setBoolsChanneling(BoolVar[] bools, SetVar set, int offset)"]}
  ([set-var bools] (set-bools-channeling set-var bools 0))
  ([set-var bools offset]
   {:pre [(integer? offset) (sequential? bools)]}
   [:constraint [:set-bools-channeling
                 [set-var
                  [:channel (vec bools)]
                  [:offset (preserve-consts offset)]]]]))

(defn sets-ints-channeling
  "Creates a constraint channeling set variables and integer variables :
  x in sets[y] <=> ints[x] = y

  Creates a constraint channeling set variables and integer variables :
  x in sets[y-offset1] <=> ints[x-offset2] = y"
  {:choco ["setsIntsChanneling(SetVar[] sets, IntVar[] ints)"
           "setsIntsChanneling(SetVar[] sets, IntVar[] ints, int offset1, int offset2)"]}
  ([sets ints] (sets-ints-channeling sets 0 ints 0))
  ([sets offset-set ints offset-int]
   {:pre [(integer? offset-int) (integer? offset-set) (sequential? ints) (sequential? sets)]}
   [:constraint [:sets-ints-channeling
                 [:sets (vec sets) :offset (preserve-consts offset-set)]
                 [:ints (vec ints) :offset (preserve-consts offset-int)]]]))
