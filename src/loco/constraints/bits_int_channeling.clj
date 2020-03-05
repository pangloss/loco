(ns loco.constraints.bits-int-channeling
  (:require
   [meander.epsilon :as m :refer [match]]
   [clojure.spec.alpha :as s]
   [clojure.walk :as walk]
   [loco.constraints.utils :refer :all :as utils]
   [loco.utils :refer [p]]
   [loco.constraints.vars :use [$bool]]
   )
  (:import
   [org.chocosolver.solver.variables IntVar BoolVar]))

(def ^:private constraint-name 'bits-int-channeling)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/tuple
                       (s/tuple #{'bits} (s/coll-of bool-var?))
                       (s/tuple #{'int-var} ::utils/coerce-intvar?)
                       ))))

(compile-function
 (let [coerce-int-var (p utils/coerce-int-var *model)]
   (match *conformed
     {:args [[_ ?bits] [_ ?int-var]]}
     (.bitsIntChanneling *model
                         (into-array BoolVar ?bits)
                         (coerce-int-var ?int-var)))))

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
       (mapv $bool bits)        ;;TODO: use $bools?
       [(constraint constraint-name
                    [['bits (vec bits)]
                     ['int-var int-var]]
                    compiler)])
      (with-meta {:generated-vars true})))
