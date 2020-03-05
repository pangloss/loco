(ns loco.constraints.arithmetic.subtraction ;; FIXME: WIP
  (:require
   [meander.epsilon :as m :refer [match]]
   [loco.constraints.utils :refer :all]
   [loco.utils :refer [p c split]]
   [loco.constraints.views.minus :use [$minus-view]]
   [loco.constraints.views.offset :use [$offset-view]]
   [loco.constraints.arithmetic.sum :use [$sum]]
   ))

(def ^:private partial-name '-)

#_(defn- name-fn [partial]
    (match partial
      [partial-name body]
      (apply str (interpose (name partial-name) body))))

(defn- constraint-fn [& partial]
  (let [[var-name [op body]] partial]
    (let [[operand1 rest-operands] ((juxt first rest) body)
          negative-vars (->> rest-operands (mapv #(if (int? %) (- %) ($minus-view %))))
          [numbers vars] (split int? rest-operands)]
      (match [operand1 vars numbers]
        [nil [] []]                        [nil]
        [(m/pred int? ?only-arg) [] []]    [(- ?only-arg)]
        [?only-arg [] []]                  [($minus-view ?only-arg)]
        [(m/pred int? ?only-arg) [] ?nums] [(- ?only-arg (apply + ?nums))]
        [?op1 [] ?nums] ($offset-view ?op1 [(- (apply + ?nums))])
        [_ _ _]                            [($sum var-name '= (apply vector operand1 negative-vars))] ;;FIXME: import $sum
        ))))

(defn- domain-fn [partial]
  (match partial
    [_partial-name ?body]
    (->
     (->> ?body
          (map domainize)
          (reduce
           (fn [{:keys [lb ub] :as acc} domain]
             (match domain
               ;;TODO: handle enumerated domains
               {:int true :lb ?cur-lb :ub ?cur-ub} {:lb (- lb ?cur-ub)
                                                    :ub (- ub ?cur-lb)}))))
     (assoc :int true))))

(defn $-
  "partial of $sum

  e.g.:
  ($= :eq ($- :n1 :n2 :n3 4)) => ($sum :eq := :n1 -:n2 -:n3 -4)
  "
  {:partial true}
  [& args]
  (partial-constraint
   partial-name
   (vec args)
   ;;:name-fn name-fn
   :constraint-fn constraint-fn
   :domain-fn domain-fn))
