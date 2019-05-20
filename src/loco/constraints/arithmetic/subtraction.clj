;; FIXME: WIP
(ns loco.constraints.arithmetic.subtraction
  (:require
   [clojure.core.match :refer [match]]
   [loco.constraints :refer [$sum]]
   ;;[loco.constraints :refer [$arithm]]
   [loco.constraints.utils :refer :all]
   [loco.constraints.views.minus :refer [$minus]]
   [loco.constraints.views.offset :refer [$offset-view]]
   [loco.utils :refer [p c split]]
   ))

(def ^:private partial-name '-)

;; (defn- name-fn [partial]
;;   (match partial
;;          [partial-name body]
;;          (apply str (interpose (name partial-name) body))))

(defn- constraint-fn [& partial]
  (let [[var-name [op body]] partial]
    (let [[operand1 rest-operands] ((juxt first rest) body)
          negative-vars (->> rest-operands (mapv #(if (int? %) (- %) ($minus %))))
          [numbers vars] (split int? rest-operands)
          return (match
                  [operand1 vars numbers]
                  [nil [] []] nil
                  [(only-arg :guard int?) [] []] (- only-arg)
                  [only-arg [] []] ($minus only-arg)
                  [(only-arg :guard int?) [] nums] (- only-arg (apply + nums))
                  [op1 [] nums] ($offset-view op1 (- (apply + nums)))
                  [_ _ _] ($sum var-name '= (apply vector operand1 negative-vars))
                  )]
      [return]
      )
    ))

(defn- domain-fn [partial]
  (match partial
         [partial-name body]
         (->
          (->> body
               (map domainize)
               (reduce
                     (fn [{:keys [lb ub] :as acc} domain]
                       (match domain
                              ;;TODO: handle enumerated domains
                              {:int true :lb cur-lb :ub cur-ub} {:lb (- lb cur-ub)
                                                                 :ub (- ub cur-lb)}))
                     ))
          (assoc :int true))))

(defloco $-
  "partial of $sum

  e.g.:
  ($= :eq ($- :n1 :n2 :n3 4)) => ($sum :eq := :n1 -:n2 -:n3 -4)
  "
  {:partial true}
  [& args]
  (partial-constraint
   partial-name
   (vec args)
   ;;name-fn
   :constraint-fn constraint-fn
   :domain-fn domain-fn))

;; ;;FIXME: subtract constraint-fn seems incomplete
;; (defn- constraint-fn [var-name [op [operand1 & more]]]
;;   (let [
;;         negative-vars (->> more
;;                            (map (fn [var-name]
;;                                   (minus var-name))))
;;         negative-var-names (map second negative-vars)
;;         ]
;;     (-> []
;;         (into negative-vars)
;;         (into [($sum var-name = (into [operand1] negative-var-names))]))))

;; ;; based on this, delete when tests pass
;; ;; (defn- subtract-domains [[lb1 ub1] [lb2 ub2]]
;; ;;   [(- lb1 ub2) (- ub1 lb2)])
;; (defn- domain-fn [partial]
;;   (match partial
;;          [partial-name body]
;;          (->
;;           (reduce
;;            (fn [{:keys [lb ub] :as acc} domain]
;;              (match domain
;;                     ;;TODO: handle enumerated domains
;;                     {:int true :lb cur-lb :ub cur-ub} {:lb (- (int lb) (int cur-ub))
;;                                                        :ub (- (int ub) (int cur-lb))}))
;;            {:lb 0 :ub 0}
;;            body)
;;           (assoc :int true))))

;; ;;TODO: $- should support negative view for 1 arity
;; (defn $-
;;   "partial of $sum

;;   e.g.:
;;   ($= :eq ($- :n1 :n2 :n3 4)) => ($sum :eq := :n1 -:n2 -:n3 -4)
;;   "
;;   {:partial true}
;;   ([& args]
;;    (partial-constraint
;;     partial-name (vec args) name-fn constraint-fn domain-fn)))
