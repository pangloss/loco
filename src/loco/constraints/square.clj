(ns loco.constraints.square
  (:use loco.constraints.utils)
  (:require
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :as utils]
   [loco.match :refer [match+]]
   [clojure.core.match :refer [match]]
   [clojure.walk :as walk])
  (:import
   [org.chocosolver.solver.variables SetVar IntVar BoolVar Task]))

(def ^:private constraint-name 'square)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/tuple int-var? int-var?))))

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:args [result dep]}
           (.square model result dep)

           ::s/invalid
           (report-spec-error constraint-name ::compile-spec var-subed-statement))))

(defn square
  "Creates a square constraint: result = dependency^2"
  {:choco "square(IntVar var1, IntVar var2)"}
  [squared int-var]
  (constraint constraint-name
              [squared int-var]
              compiler))
