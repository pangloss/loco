(ns loco.constraints.times
  (:use loco.constraints.utils)
  (:require
   [clojure.spec.alpha :as s]
   [clojure.core.match :refer [match]]
   [clojure.walk :as walk])
  (:import
   [org.chocosolver.solver.variables IntVar BoolVar]))

(def ^:private constraint-name 'times)

(s/def ::compile-spec
  (s/cat :constraint #{'times}
         :args (s/spec
                (s/tuple int-var? #{'=} int-var? #{'*} int-var?))))

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:args [eq-var _ operand1 _ operand2]}
           (.times model operand1 operand2 eq-var)

           ::s/invalid
           (report-spec-error constraint-name ::compile-spec var-subed-statement))))

(defloco $times
  "Creates a multiplication constraint:

  eq = operand1 * operand2

  eq         = IntVar
  operand1   = IntVar
  operand2   = IntVar"

  {:choco "times(IntVar X, IntVar Y, IntVar Z)"}
  ([eq = operand1 * operand2] ($times eq operand1 operand2))
  ([eq operand1 operand2]
   (constraint constraint-name
    [eq '= operand1 '* operand2]
    compiler)))

;; -------------------- partial --------------------

(def ^:private partial-name '*)

(defn- name-fn [partial]
  (match partial
         [partial-name body]
         (apply str (interpose (name partial-name) body))))

(defn- constraint-fn [var-name [op [operand1 operand2]]]
  ($times var-name = operand1 * operand2))

(defn- domain-fn [[partial-name [{lb1 :lb ub1 :ub} {lb2 :lb ub2 :ub}]]]
  (let [possible-bounds [(* lb1 lb2)
                         (* lb1 ub2)
                         (* ub1 lb1)
                         (* ub1 ub2)]]
    {:int true
     :lb (-> (apply min possible-bounds) int)
     :ub (-> (apply max possible-bounds) int)}))

(defloco $*
  "partial of $times

  allows for unlimited args (will create recursive constraint to support args)

  e.g.:
  ($= :eq ($* :n1 :n2)) => ($times :eq = :n1 * :n2)"
  {:partial true}
  [& args]
  (match (vec args)
         [x y] (partial-constraint partial-name [x y] name-fn constraint-fn domain-fn)
         [x & more] (partial-constraint
                     partial-name [x (apply $* more)] name-fn constraint-fn domain-fn)))
