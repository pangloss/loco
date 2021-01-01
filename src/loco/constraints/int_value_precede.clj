(ns loco.constraints.int-value-precede
  (:require
   [meander.epsilon :as m :refer [match]]
   [clojure.spec.alpha :as s]
   [clojure.walk :as walk]
   [loco.constraints.utils :refer :all :as utils]
   )
  (:import
   [org.chocosolver.solver.variables SetVar IntVar BoolVar Task]))

(def ^:private constraint-name 'int-value-precede)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/tuple
                       ::utils/coll-coerce-intvar?
                       int?
                       int?))))

(compile-function compiler constraint-name [*conformed *model]
 (let [coerce-var (utils/coerce-var *model)]
   (match *conformed
     {:args [?variables ?s ?t]}
     (.intValuePrecedeChain
      *model
      (->> ?variables (map coerce-var) (into-array IntVar))
      ?s ?t))))

(defn $int-value-precede
  "CHOCO:
intValuePrecedeChain(IntVar[] X, int S, int T)
Ensure that if there exists j such that X[j] = T, then, there must
exist i < j such that X[i] = S.

GCCAT: 
Arguments
    𝚂	𝚒𝚗𝚝
    𝚃	𝚒𝚗𝚝
    𝚅𝙰𝚁𝙸𝙰𝙱𝙻𝙴𝚂	𝚌𝚘𝚕𝚕𝚎𝚌𝚝𝚒𝚘𝚗(𝚟𝚊𝚛-𝚍𝚟𝚊𝚛)

Restrictions
    𝚂≠𝚃
    𝚛𝚎𝚚𝚞𝚒𝚛𝚎𝚍(𝚅𝙰𝚁𝙸𝙰𝙱𝙻𝙴𝚂,𝚟𝚊𝚛)

Purpose 
    If value 𝚃 occurs in the collection of variables 𝚅𝙰𝚁𝙸𝙰𝙱𝙻𝙴𝚂
    then its first occurrence should be preceded by an occurrence of
    value 𝚂.

Example
    ((4,0,6,1,0), 0, 1)

    The 𝚒𝚗𝚝_𝚟𝚊𝚕𝚞𝚎_𝚙𝚛𝚎𝚌𝚎𝚍𝚎 constraint holds since the first occurrence
    of value 0 precedes the first occurrence of value 1."
{:choco ["intValuePrecedeChain(IntVar[] X, int S, int T)"]
 :gccat ["http://sofdem.github.io/gccat/gccat/Cint_value_precede.html"]}
  ([variables s t]
   {:pre [(int? s) (int? t) (sequential? variables)]}
   (constraint constraint-name
               [(vec variables) s t]
               compiler)))

