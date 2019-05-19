;; FIXME: WIP

(ns loco.constraints.max
  (:use loco.constraints.utils)
  (:require
   [loco.utils :refer [p c]]
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :as utils]
   [loco.match :refer [match+]]
   [clojure.core.match :refer [match]]
   [clojure.walk :as walk])
  (:import
   [org.chocosolver.solver.variables SetVar IntVar BoolVar Task]))

(def ^:private constraint-name 'max)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/or
                       :bools       (s/tuple
                                     bool-var?
                                     (s/tuple #{'of}         (s/coll-of bool-var?)))

                       :ints        (s/tuple
                                     int-var?
                                     (s/tuple #{'of}         (s/coll-of int-var?)))

                       :set         (s/tuple
                                     int-var?
                                     (s/tuple #{'of}         set-var?)
                                     (s/tuple #{'not-empty?} boolean?))

                       :set-indices (s/tuple
                                     int-var?
                                     (s/tuple #{'of}         (s/coll-of int?))
                                     (s/tuple #{'indices}    set-var?)
                                     (s/tuple #{'offset}     nat-int?)
                                     (s/tuple #{'not-empty?} boolean?))
                       ))))

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:args [(:or :ints :bools) [max [_ vars]]]}
           (.max model max (into-array vars))

           ;;{:args [:bools [max [_ vars]]]}

           {:args [:set [max [_ set] [_ not-empty?]]]}
           (.max model set max not-every?) ;;fugly API! bad choco!

           {:args [:set-indices [max [_ weights] [_ indices] [_ offset] [_ not-empty?]]]}
           (.max model indices (int-array weights) offset max not-every?)

           ::s/invalid
           (report-spec-error constraint-name ::compile-spec var-subed-statement))))

(defn- name-fn [partial]
  (match partial
         [partial-name body]
         (->> (interpose "_" (map name body))
              (apply str (name partial-name) "_"))))

(declare $max)
(defn- constraint-fn [var-name [op args]]
  ($max var-name args))

(defn- domain-fn [partial]
  (match partial
         [partial-name body]
         (->
          (reduce
           (fn [{:keys [lb ub] :as acc} domain]
             (match domain
                    ;;TODO: handle enumerated domains
                    {:int true :lb d-lb :ub d-ub} {:lb (max lb d-lb)
                                                   :ub (max ub d-ub)}))
           ;;{:lb 0 :ub 0}
           body)
          (assoc :int true)
          (update :lb int)
          (update :ub int))))

(defn- max-partial
  "handles syntax like ($= :v ($max :a :b :c))"
  [vars]
  {:pre [(sequential? vars)]}
  (partial-constraint constraint-name (vec vars) name-fn constraint-fn domain-fn))

;;TODO: redo max and min docs, this is complicated...
;;TODO: replace defun with match+ or conform add bool documentation
;;TODO: fix arglists

(defloco $max
  "The maximum of several arguments.
  The arguments can be a mixture of int-vars and numbers
  Creates a constraint over the maximum element in a set: max{i | i in set} = maxElementValue
  Creates a constraint over the maximum element induces by a set: max{weights[i-offset] | i in indices} = maxElementValue"
  {:choco
   ["max(IntVar max, IntVar[] vars)"
    "max(BoolVar max, BoolVar[] vars)"
    "max(SetVar set, IntVar maxElementValue, boolean notEmpty)"
    "max(SetVar indices, int[] weights, int offset, IntVar maxElementValue, boolean notEmpty)"]
   :partial true
   :arglists '([max-list]
               [max vars]
               [set-var max not-empty?]
               [set-indices weights<int[]> offset<int> max<int-var> not-empty?]
               [& int-vars])}
  [& more]
  (match
   (vec more)

   [(max-list :guard sequential?)] (apply max-partial max-list)

   [max (vars :guard sequential?)]
   (constraint constraint-name
               [max
                ['of (vec vars)]] compiler)

   [set-var max (not-empty? :guard boolean?)]
   (constraint constraint-name
               [max
                ['of set-var]
                ['not-empty? not-empty?]]
               compiler)

   [set-indices,
    (weights :guard [sequential? (p every? int?)])
    (offset :guard integer?)
    max,
    (not-empty? :guard boolean?)]
   (constraint constraint-name
               [max
                ['of          (vec weights)]
                ['indices    set-indices]
                ['offset      offset]
                ['not-empty? not-empty?]]
               compiler)

   [& int-vars] (apply max-partial int-vars)))
