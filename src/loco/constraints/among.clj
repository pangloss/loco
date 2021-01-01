(ns loco.constraints.among
  (:require
   [meander.epsilon :as m :refer [match]]
   [clojure.spec.alpha :as s]
   [clojure.walk :as walk]
   [loco.constraints.utils :refer :all :as utils]
   [loco.utils :refer [p]]
   )
  (:import
   [org.chocosolver.solver.variables IntVar]))

(def ^:private constraint-name 'among)

;;example: [among [[:a :b :c] [nb-var 3] [values [2 3]]]]
(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/cat
                       :ints   ::utils/coll-intvar?
                       :nb-var (s/tuple #{'nb-var} ::utils/coerce-intvar?)
                       :values (s/tuple #{'values} (s/coll-of int?))))))

(compile-function compiler constraint-name [*conformed *model]
 (let [coerce-int-var (p utils/coerce-int-var *model)]
   (match *conformed
     {:args {:ints ?vars :nb-var [_ ?nb-var] :values [_ ?values]} }
     (.among *model
             (coerce-int-var ?nb-var)
             (->> ?vars (into-array IntVar))
             (int-array ?values)))))

(defn $among
  "Creates an among constraint.
  nb-var is the number of variables of the collection vars that take their value in values."
  {:choco "among(IntVar nbVar, IntVar[] vars, int[] values)"
   :gccat "http://www.emn.fr/x-info/sdemasse/gccat/Camong.html"}
  [nb-var vars values]
  {:pre [(sequential? vars) (sequential? values) (every? int? values)]}

  (constraint constraint-name
              [(vec vars)
               ['nb-var nb-var]
               ['values (vec values)]]
              compiler))
