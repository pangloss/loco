(ns loco.constraints.square

  (:require
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :refer :all :as utils]
   [meander.epsilon :as m :refer [match]]
   [clojure.walk :as walk])
  (:import
   [org.chocosolver.solver.variables SetVar IntVar BoolVar Task]))

(def ^:private constraint-name 'square)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/tuple int-var? int-var?))))

(compile-function compiler constraint-name [*conformed *model]
 (match *conformed
   {:args [?result ?dep]} (.square *model ?result ?dep)))

(defn $square
  "Creates a square constraint: result = dependency^2"
  {:choco "square(IntVar var1, IntVar var2)"}
  [squared int-var]
  (constraint constraint-name
              [squared int-var]
              compiler))
