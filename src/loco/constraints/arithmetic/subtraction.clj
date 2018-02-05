(in-ns 'loco.constraints.arithmetic.subtraction)
(ns loco.constraints.arithmetic
  (:use loco.utils
        loco.constraints.utils)
  (:require [clojure.core.match :refer [match]]
            [loco.constraints.sum :refer [$sum]]))

(def ^:private partial-name '-)

(defn- name-fn [partial]
  (match partial
         [partial-name body]
         (apply str (interpose (name partial-name) body))))

[var-name [:- [arg1 & more]]]


(defn- constraint-fn [var-name [op [operand1 & more]]]
  (let [
        negative-vars (->> more
                           (map (fn [var-name]
                                  ;;TODO: fuck, need negative views here to make this not suck
                                  ^{:neg var-name} [:var (neg-var-name var-name) :proto])))
        negative-var-names (map second negative-vars)
        ]
    (-> []
        (into negative-vars)
        (into [statement])
        (into [($sum var-name = (into [operand1] negative-var-names))]))))

;; based on this, delete when tests pass
;; (defn- subtract-domains [[lb1 ub1] [lb2 ub2]]
;;   [(- lb1 ub2) (- ub1 lb2)])
(defn- domain-fn [partial]
  (match partial
         [partial-name body]
         (->
          (reduce
           (fn [{:keys [lb ub] :as acc} domain]
             (match domain
                    ;;TODO: handle enumerated domains
                    {:int true :lb cur-lb :ub cur-ub} {:lb (- (int lb) (int cur-ub))
                                                       :ub (- (int ub) (int cur-lb))}))
           {:lb 0 :ub 0}
           body)
          (assoc :int true))))

(defn $-
  "partial of $sum

  e.g.:
  ($= :eq ($- :n1 :n2 :n3 4)) => ($sum :eq := :n1 -:n2 -:n3 -4)
  "
  {:partial true}
  ([& args]
   (partial-constraint
    partial-name (vec args) name-fn constraint-fn domain-fn)))
