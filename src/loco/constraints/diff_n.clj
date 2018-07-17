(ns loco.constraints.diff-n
  (:use loco.constraints.utils)
  (:require
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :as utils]
   [loco.match :refer [match+]]
   [clojure.core.match :refer [match]]
   [clojure.walk :as walk])
  (:import
   org.chocosolver.solver.constraints.nary.cumulative.Cumulative$Filter
   [org.chocosolver.solver.variables SetVar IntVar BoolVar Task]))

(def ^:private constraint-name 'diff-n)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/tuple
                       (s/tuple #{'xs}                        (s/coll-of int-var?))
                       (s/tuple #{'ys}                        (s/coll-of int-var?))
                       (s/tuple #{'widths}                    (s/coll-of int-var?))
                       (s/tuple #{'heights}                   (s/coll-of int-var?))
                       (s/tuple #{'add-cumulative-reasoning}  boolean?)
                       ))))

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:args [[_ xs] [_ ys] [_ widths] [_ heights] [_ add-cumulative-reasoning?]]}
           (.diffN model
                   (into-array IntVar xs)
                   (into-array IntVar ys)
                   (into-array IntVar widths)
                   (into-array IntVar heights)
                   add-cumulative-reasoning?)

           ::s/invalid
           (report-spec-error constraint-name ::compile-spec var-subed-statement))))

;;TODO: implement diff-n with object args, as opposed to parallel array args
;;[{:x :y :width :height} ...]
(defloco $diff-n
  "Creates a diffN constraint.
  Constrains each rectangle[i],
  given by their origins X[i],Y[i] and sizes width[i], height[i],
  to be non-overlapping.

  GCCAT:
  http://sofdem.github.io/gccat/gccat/Cdiffn.html"
  {:choco "diffN(IntVar[] X, IntVar[] Y, IntVar[] width, IntVar[] height, boolean addCumulativeReasoning)"
   :gccat "http://sofdem.github.io/gccat/gccat/Cdiffn.html"}
  [xs ys widths heights add-cumulative-reasoning?]
  {:pre [(every? sequential? [xs ys widths heights]) (boolean? add-cumulative-reasoning?)]}
  (constraint constraint-name
              [['xs xs]
               ['ys ys]
               ['widths widths]
               ['heights heights]
               ['add-cumulative-reasoning add-cumulative-reasoning?]]
              compiler))
