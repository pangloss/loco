(in-ns 'loco.constraints)
(ns loco.views.minus
  (:require
   [clojure.walk :as walk]
   [clojure.core.match :refer [match]]
   [clojure.spec.alpha :as s]
   [clojure.set :as set]
;;   [loco.constraints.vars :as vars]
   )
  (:use loco.constraints.utils))

(def ^:private view-name 'minus)

(s/def ::compile-spec
  (s/tuple #{view-name}
           #(or (string? %) (keyword? %))
           int-var?))

;;TODO: this requires that there is a similar var compiler function as to constraints
(defn- compiler-fn [model vars-index statement]
  (let [constraint-name view-name
        var-subed-statement (->> statement (walk/prewalk-replace vars-index))]
    (match (->> var-subed-statement (s/conform ::neg-compile-spec))
           {:args [result name dependency-var]}
           (.minusView model dependency-var)

           ::s/invalid
           (report-spec-error view-name ::compile-spec var-subed-statement))))

(defn- domain-fn [partial]
  (match partial
         [view-name [dependency]]
         (-> (match dependency
                    ;;TODO: handle enumerated domains
                    {:int true :lb lb :ub ub}
                    (let [[neg-lb neg-ub] (sort [(- lb) (- ub)])] {:lb neg-lb :ub neg-ub}))
             (assoc :int true)
             (update :lb int)
             (update :ub int))))

;; if i set this up as a partial constraint, then i don't have to
;; worry about adding new logic to the code to support views (which
;; are a sorta mix of constraint/var). it may be possible that we can
;; completely treat this like a partial-constraint, as it just outputs
;; a proto

;;TODO: fix up docs
(defn $minus
  "takes a partial constraint and creates a negative constraint from
  it (neg (- :x :b)) also can be used to create a neg var
  via (neg :-i :i)

  :exmaple
  ($int :my-var 0 5)
  ($neg :my-var) -> [:var :-my-var [:int -5 0]]
  "
  {:view true}
  ([dependency-name]
   (view view-name
         [dependency-name]
         compiler-fn
         domain-fn)))

;;TODO: add meta data
(def $neg (partial $minus))
