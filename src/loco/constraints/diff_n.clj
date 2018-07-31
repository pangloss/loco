(ns loco.constraints.diff-n
  (:require
   [clojure.core.match :refer [match]]
   [clojure.spec.alpha :as s]
   [clojure.walk :as walk]
   [loco.constraints.utils :refer :all :as utils]
   [loco.utils :refer [p c]]
   )
  (:import
   [org.chocosolver.solver.variables SetVar IntVar BoolVar]))

(def ^:private constraint-name 'diff-n)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/tuple
                       (s/tuple #{'rects} (s/coll-of
                                           (s/tuple #{'x} ::utils/coerce-intvar?
                                                    #{'y} ::utils/coerce-intvar?
                                                    #{'w} ::utils/coerce-intvar?
                                                    #{'h} ::utils/coerce-intvar?)))
                       (s/tuple #{'add-cumulative-reasoning} boolean?)))))

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))
        coerce-var (p utils/coerce-var model)]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:args [[_ rects] [_ add-cumulative-reasoning?]]}
           (let [xs (map (c coerce-var #(nth % 1)) rects)
                 ys (map (c coerce-var #(nth % 3)) rects)
                 widths (map (c coerce-var #(nth % 5)) rects)
                 heights (map (c coerce-var #(nth % 7)) rects)
                 ]
             (.diffN model
                     (into-array IntVar xs)
                     (into-array IntVar ys)
                     (into-array IntVar widths)
                     (into-array IntVar heights)
                     add-cumulative-reasoning?))

           ::s/invalid
           (report-spec-error constraint-name ::compile-spec var-subed-statement))))

(defloco $diff-n
  "Creates a diffN constraint.
  Constrains each rectangle[i],
  given by their origins X[i],Y[i] and sizes width[i], height[i],
  to be non-overlapping.

  GCCAT:
  http://sofdem.github.io/gccat/gccat/Cdiffn.html

The diffn constraint occurs in placement and scheduling problems. It
was for instance used for scheduling problems where one has to both
assign each non-preemptive task to a resource and fix its origin so
that two tasks, which are assigned to the same resource, do not
overlap. When the resource is a set of persons to which non-preemptive
tasks have to be assigned this corresponds to so called timetabling
problems. A second practical application from the area of the design
of memory-dominated embedded systems [Szymanek04] can be found
in [SzymanekKuchcinski01]. Together with arithmetic and cumulative
constraints, the diffn constraint was used in [Szczygiel01] for
packing more complex shapes such as angles. Figure 5.118.4 illustrates
the angle packing problem on an instance involving 10 angles taken
from [Szczygiel01].
"

  {:choco "diffN(IntVar[] X, IntVar[] Y, IntVar[] width, IntVar[] height, boolean addCumulativeReasoning)"
   :gccat "http://sofdem.github.io/gccat/gccat/Cdiffn.html"}
  ([rects add-cumulative-reasoning?]
   {:pre [(vector? rects) (every? vector? rects) (every? #(= 4 (count %)) rects)
          (boolean? add-cumulative-reasoning?)]}
   (constraint constraint-name
               [['rects (mapv (c vec (p interleave ['x 'y 'w 'h])) rects)]
                ['add-cumulative-reasoning add-cumulative-reasoning?]]
               compiler)))
