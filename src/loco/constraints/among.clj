(ns loco.constraints.among
  (:require
   [clojure.core.match :refer [match]]
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

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))
        coerce-int-var (p utils/coerce-int-var model)]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:args {:ints vars :nb-var [_ nb-var] :values [_ values]} }
           (.among model
                   (coerce-int-var nb-var)
                   (->> vars (into-array IntVar))
                   (int-array values))

           ::s/invalid
           (report-spec-error constraint-name ::compile-spec var-subed-statement))))


(defloco $among
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
