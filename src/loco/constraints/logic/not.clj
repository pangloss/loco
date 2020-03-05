(ns loco.constraints.logic.not ;;FIXME: finish implementing
  (:require
   [meander.epsilon :as m :refer [match]]
   [clojure.spec.alpha :as s]
   [clojure.walk :as walk]
   [loco.constraints.utils :refer :all :as utils]
   )
  (:import
   org.chocosolver.solver.constraints.Constraint
   ))

(def ^:private constraint-name 'not)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         ;;FIXME: change this
         :args (s/spec
                constraint?)))

;;TODO: in compiler.clj we handle logical constraints (like this) with a recursive function
;; example:
;; [:and (constraints :guard (p every? constraint?))]
;; (.and model (realize-nested-constraints constraints))
(compile-function
 (match *conformed
   {:args ?constraint} (.not *model ?constraint)))

(defn $not
  "Given a constraint C, returns \"not C\" a.k.a. \"~C\", which is true iff C is false."
  {:choco "not(Constraint cstr)"}
  [constraint]
  (constraint constraint-name constraint compiler))
