(ns loco.constraints.set.nb-empty
  (:use loco.constraints.utils)
  (:require
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :as utils]
   [loco.match :refer [match+]]
   [clojure.core.match :refer [match]]
   [clojure.walk :as walk])
  (:import
   [org.chocosolver.solver.variables SetVar IntVar BoolVar Task]))

(def ^:private constraint-name 'set/nb-empty)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/tuple
                       int-or-intvar?
                       (s/tuple #{'of} (s/coll-of set-var?))))))

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:args [num-empty ['of sets]]}
           (.nbEmpty model (into-array SetVar sets) num-empty)

           ::s/invalid
           (report-spec-error constraint-name ::compile-spec var-subed-statement))))

(defloco $nb-empty
  "Creates a constraint counting the number of empty sets sets |{s in sets where |s|=0}| = nbEmpty"
  {:choco ["nbEmpty(SetVar[] sets, int nbEmpty)"
           "nbEmpty(SetVar[] sets, IntVar nbEmpty)"]}
  [num-empty-sets collection]
  {:pre [(sequential? collection)]}
  (constraint constraint-name
              [ num-empty-sets
               ['of (vec collection)]]
              compiler))

(defloco $count-empty [& more] (apply $nb-empty more))
(reset-meta! (var $count-empty) (meta (var $nb-empty)))
