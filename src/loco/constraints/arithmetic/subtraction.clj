(ns loco.constraints.arithmetic.subtraction
  (:require
   [loco.constraints.utils :refer :all]
   [clojure.core.match :refer [match]]
   [loco.constraints :refer [$arithm $sum]]
   [loco.constraints.views.minus :refer [$minus]]
   ))

(def ^:private partial-name '-)

(defn- name-fn [partial]
  (match partial
         [partial-name body]
         (apply str (interpose (name partial-name) body))))

(defn- constraint-fn [var-name [op [operand1 & more]]]
  (if (empty? more)
    [($arithm var-name = ($minus operand1))]
    (let [negative-vars (->> more (mapv $minus))]
      [($sum var-name = (into [operand1] negative-vars))])))

(defn- domain-fn [partial]
  (match partial
         [partial-name body]
         (->
          (reduce
           (fn [{:keys [lb ub] :as acc} domain]
             (match domain
                    ;;TODO: handle enumerated domains
                    {:int true :lb cur-lb :ub cur-ub}
                    (do
                      {:lb (unchecked-subtract (int lb) (int cur-ub))
                       :ub (unchecked-subtract (int ub) (int cur-lb))})))
           body)
          (assoc :int true))))

(defloco $-
  "partial of $sum

  e.g.:
  ($= :eq ($- :n1 :n2 :n3 4)) => ($sum :eq := :n1 -:n2 -:n3 -4)
  "
  {:partial true}

  ([& args]
   {:pre [(not-empty args)]}
   (if (= 1 (count args))
     ($minus (first args))
     (partial-constraint
      partial-name (vec args) name-fn constraint-fn domain-fn))))
