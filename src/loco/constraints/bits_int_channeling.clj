(in-ns 'loco.constraints)
(ns loco.constraints.bits-int-channeling
  (:require
   [loco.constraints.utils :refer [constraint]]
   [loco.constraints :refer [$bool]]
   [clojure.spec.alpha :as s]
   [clojure.core.match :refer [match]]
   [clojure.walk :as walk])
  (:import
   [org.chocosolver.solver.variables IntVar BoolVar]))

(def ^:private constraint-name 'bits-int-channeling)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/tuple
                       (s/tuple #{'bits} (s/coll-of bool-var?))
                       (s/tuple #{'int-var} int-var?)
                       ))))

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:args [[_ bits] [_ int-var]]}
           (.bitsIntChanneling model (into-array BoolVar bits) int-var)

           ::s/invalid
           (report-spec-error constraint-name ::compile-spec var-subed-statement))))

(defn $bits-int-channeling
  "Creates an channeling constraint between an integer variable and a set of bit variables.
  Ensures that var = 20*BIT_1 + 21*BIT_2 + ... 2n-1*BIT_n.

  BIT_1 is related to the first bit of OCTET (2^0), BIT_2 is related
  to the first bit of OCTET (2^1), etc.  The upper bound of var is
  given by 2n, where n is the size of the array bits."
  {:choco "bitsIntChanneling(BoolVar[] bits, IntVar var)"}
  [bits int-var]
  {:pre [(sequential? bits)]}
  (-> (concat
       ;;TODO: use $bools?
       (mapv $bool bits)
       [(constraint constraint-name
                    [['bits (vec bits)]
                     ['int-var int-var]]
                    compiler)])
      (with-meta {:generated-vars true})))
