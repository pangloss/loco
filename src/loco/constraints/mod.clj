(ns loco.constraints.mod
  (:refer-clojure :exclude [mod])
  (:use loco.constraints.utils)
  (:require
   [clojure.spec.alpha :as s]
   [loco.utils :refer [p c]]
   [loco.constraints.utils :as utils]
   [clojure.core.match :refer [match]]
   [loco.match :refer [match+]]
   [clojure.walk :as walk]))

(def ^:private constraint-name 'mod)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args (s/spec
                (s/tuple int-var? #{'=} int-var? #{'%} int-var?))))

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:args [eq-var _ operand1 _ operand2]}
           (.mod model operand1 operand2 eq-var)

           ::s/invalid
           (utils/report-spec-error constraint-name ::compile-spec var-subed-statement))))

(defn mod
  "Creates a modulo constraint.

  eq = operand1 % operand2

  eq         = IntVar
  operand1   = IntVar
  operand2   = IntVar"
  {:choco "mod(IntVar X, IntVar Y, IntVar Z)"
   :partial true}
  ([eq = operand1 _% operand2] (mod eq operand1 operand2))
  ([eq operand1 operand2]
   (-> (constraint [constraint-name [eq '= operand1 '% operand2]])
       (with-compiler compiler)))
  ([operand1 operand2]
   (partial-constraint [:% [operand1 operand2]])))

(def % mod)
(reset-meta! (var %) (meta (var mod)))
