(ns loco.constraints.int-value-precede-chain
  (:require
   [clojure.core.match :refer [match]]
   [clojure.spec.alpha :as s]
   [clojure.walk :as walk]
   [loco.constraints.utils :refer :all :as utils]
   )
  (:import
   [org.chocosolver.solver.variables SetVar IntVar BoolVar Task]))

(def ^:private constraint-name 'int-value-precede-chain)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/tuple
                       ::utils/coll-coerce-intvar?
                       (s/coll-of int?)))))

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))
        coerce-var (utils/coerce-var model)]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:args [variables values]}
           (.intValuePrecedeChain
            model
            (->> variables (map coerce-var) (into-array IntVar))
            (int-array values))

           ::s/invalid
           (report-spec-error constraint-name ::compile-spec var-subed-statement))))

(defloco $int-value-precede-chain
  "CHOCO:
intValuePrecedeChain(IntVar[] X, int[] V)
Ensure that, for each pair of V[k] and V[l] of values in V, such that
k < l, if there exists j such that X[j] = V[l], then, there must exist
i < j such that X[i] = V[k].  i < j such that X[i] = V[k].

GCCAT:
Arguments
    𝚅𝙰𝙻𝚄𝙴𝚂	𝚌𝚘𝚕𝚕𝚎𝚌𝚝𝚒𝚘𝚗(𝚟𝚊𝚛-𝚒𝚗𝚝)
    𝚅𝙰𝚁𝙸𝙰𝙱𝙻𝙴𝚂	𝚌𝚘𝚕𝚕𝚎𝚌𝚝𝚒𝚘𝚗(𝚟𝚊𝚛-𝚍𝚟𝚊𝚛)

Restrictions
    𝚛𝚎𝚚𝚞𝚒𝚛𝚎𝚍(𝚅𝙰𝙻𝚄𝙴𝚂,𝚟𝚊𝚛)
    𝚍𝚒𝚜𝚝𝚒𝚗𝚌𝚝(𝚅𝙰𝙻𝚄𝙴𝚂,𝚟𝚊𝚛)
    𝚛𝚎𝚚𝚞𝚒𝚛𝚎𝚍(𝚅𝙰𝚁𝙸𝙰𝙱𝙻𝙴𝚂,𝚟𝚊𝚛)

Purpose
    Assuming n denotes the number of items of the 𝚅𝙰𝙻𝚄𝙴𝚂 collection,
    the following condition holds for every i∈[1,n-1]: When it is
    defined, the first occurrence of the (i+1)th value of the 𝚅𝙰𝙻𝚄𝙴𝚂
    collection should be preceded by the first occurrence of the ith
    value of the 𝚅𝙰𝙻𝚄𝙴𝚂 collection.

Example
    (〈4,0,6,1,0〉, 〈4,0,1〉)

    The 𝚒𝚗𝚝_𝚟𝚊𝚕𝚞𝚎_𝚙𝚛𝚎𝚌𝚎𝚍𝚎_𝚌𝚑𝚊𝚒𝚗 constraint holds since within the sequence (4, 0, 6, 1, 0):
    - The first occurrence of value 4 occurs before the first occurrence of value 0.
    - The first occurrence of value 0 occurs before the first occurrence of value 1."
  {:choco ["intValuePrecedeChain(IntVar[] X, int[] V)"]
   :gccat ["http://sofdem.github.io/gccat/gccat/Cint_value_precede_chain.html"]}
  ([variables values]
   {:pre [(sequential? values) (distinct? values) (every? int? values) (sequential? variables)]}
   (constraint constraint-name
               [(vec variables) (vec values)]
               compiler)))
