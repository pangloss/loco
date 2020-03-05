(ns loco.constraints.mod

  (:require
   [clojure.spec.alpha :as s]
   [loco.utils :refer [p c]]
   [loco.constraints.utils :refer :all :as utils]
   [meander.epsilon :as m :refer [match]]

   [clojure.walk :as walk]))

(def ^:private constraint-name 'mod)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args (s/spec
                (s/tuple int-var? #{'=} int-var? #{'%} int-var?))))

(compile-function
 (match *conformed
   {:args [?eq-var _ ?operand1 _ ?operand2]}
   (.mod *model ?operand1 ?operand2 ?eq-var)))

(defn $mod
  "Creates a modulo constraint.

  eq = operand1 % operand2

  eq         = IntVar
  operand1   = IntVar
  operand2   = IntVar"
  {:choco "mod(IntVar X, IntVar Y, IntVar Z)"
   :partial true}
  ([eq = operand1 _% operand2] ($mod eq operand1 operand2))
  ([eq operand1 operand2]
   (constraint constraint-name
               [eq '= operand1 '% operand2]
               compiler))
  ([operand1 operand2]
   (partial-constraint ['% [operand1 operand2]])))

;; -------------------- partial --------------------

(def ^:private partial-name '%)

(defn- name-fn [partial]
  (match partial
    [?partial-name ?body]
    (apply str (interpose (name ?partial-name) ?body))))

(defn- constraint-fn [var-name [op [operand1 operand2]]]
  ($mod var-name = operand1 '% operand2))

;; TODO: delete this when $% tests are passing
;; (defn modulo-domains [[lb1 ub1] [lb2 ub2]]
;;   [0 ub2])

(defn domain-fn [partial [_ {ub2 :ub}]]
  (-> {:lb 0 :ub ub2}
      (assoc :int true)
      (update :lb int)
      (update :ub int)))

(defn $%
  "partial of $mod

  e.g.:
  ($= :eq ($% :n1 :n2)) => ($mod :eq '% :n1 :n2)
  "
  {:partial true}
  ([operand1 operand2]
   (partial-constraint
    partial-name
    [operand1 operand2] ;; body
    name-fn
    constraint-fn
    domain-fn)))
