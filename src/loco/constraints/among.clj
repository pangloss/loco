(in-ns 'loco.constraints)
(ns loco.constraints.among
  (:use loco.constraints.utils)
  (:require
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :as utils]
   [loco.match :refer [match+]]
   [clojure.core.match :refer [match]]
   [clojure.walk :as walk])
  (:import
   [org.chocosolver.solver.variables IntVar]))

(def ^:private constraint-name 'among)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/cat
                       :ints   (s/coll-of int-var?)
                       :nb-var (s/spec (s/tuple #{'nb-var} int-var?))
                       :values (s/spec (s/tuple #{'values} (s/spec (s/coll-of int?))))))))

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:args {:ints vars :nb-var [_ nb-var] :values [_ values]} }
           (.among model
                   nb-var
                   (into-array IntVar vars)
                   (int-array values))

           ::s/invalid
           (report-spec-error constraint-name ::compile-spec var-subed-statement))))


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
               ['values (preserve-consts (vec values))]]
              compiler))
