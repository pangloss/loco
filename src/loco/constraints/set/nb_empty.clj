(ns loco.constraints.set.nb-empty

  (:require
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :refer :all :as utils]

   [meander.epsilon :as m :refer [match]]
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

(compile-function
 (match *conformed
   {:args [?num-empty ['of ?sets]]}
   (.nbEmpty *model (into-array SetVar ?sets) ?num-empty)))

(defn $count-empty
  "Creates a constraint counting the number of empty sets |{s in sets where |s|=0}| = nbEmpty"
  {:choco ["nbEmpty(SetVar[] sets, int nbEmpty)"
           "nbEmpty(SetVar[] sets, IntVar nbEmpty)"]}
  [num-empty-sets collection]
  {:pre [(sequential? collection)]}
  (constraint constraint-name
              [ num-empty-sets
               ['of (vec collection)]]
              compiler))
