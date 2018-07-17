(ns loco.constraints.arithmetic
  (:require
   [loco.constraints.utils :refer :all]
   [clojure.core.match :refer [match]]
   [loco.constraints :refer [$sum $minus]]))

(def ^:private partial-name '-)

(defn- name-fn [partial]
  (match partial
         [partial-name body]
         (apply str (interpose (name partial-name) body))))

;;FIXME: subtract constraint-fn seems incomplete
(defn- constraint-fn [var-name [op [operand1 & more]]]
  (let [
        negative-vars (->> more
                           (map (fn [var-name]
                                  ($minus var-name))))
        ;;negative-var-names (map second negative-vars)
        ]
    [($sum var-name = (into [operand1] negative-vars))]))

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

;;TODO: $- should support negative view for 1 arity
(defloco $-
  "partial of $sum

  e.g.:
  ($= :eq ($- :n1 :n2 :n3 4)) => ($sum :eq := :n1 -:n2 -:n3 -4)
  "
  {:partial true}
  ([& args]
   (partial-constraint
    partial-name (vec args) name-fn constraint-fn domain-fn)))
