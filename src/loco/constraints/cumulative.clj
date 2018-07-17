(ns loco.constraints.cumulative
  (:use loco.constraints.utils)
  (:require
   [loco.utils :refer [p c]]
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :as utils]
   [loco.match :refer [match+]]
   [clojure.core.match :refer [match]]
   [clojure.walk :as walk])
  (:import
   org.chocosolver.solver.constraints.nary.cumulative.Cumulative$Filter
   [org.chocosolver.solver.variables SetVar IntVar BoolVar Task]))

(def ^:private constraint-name 'cumulative)

(def ^:private filter-to-enum
  {
   'default                   Cumulative$Filter/DEFAULT
   'disjunctive-task-interval Cumulative$Filter/DISJUNCTIVE_TASK_INTERVAL
   'heights                   Cumulative$Filter/HEIGHTS
   'nrj                       Cumulative$Filter/NRJ
   'sweep                     Cumulative$Filter/SWEEP
   'sweep-hei-sort            Cumulative$Filter/SWEEP_HEI_SORT
   'time                      Cumulative$Filter/TIME
   })

(def ^:private filters? (set (concat
                              (keys filter-to-enum)
                              (map keyword (keys filter-to-enum))
                              (map str (keys filter-to-enum)))))

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/tuple
                       (s/coll-of               task-var?)
                       (s/tuple #{'heights}     (s/coll-of int-var?))
                       (s/tuple #{'capacity}    int-var?)
                       (s/tuple #{'incremental} boolean?)
                       (s/tuple #{'filters}     (s/coll-of filters?))
                       ))))

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:args [tasks [_ heights] [_ capacity] [_ incremental?] [_ filters]]}
           (.cumulative model
                        (into-array Task tasks)
                        (into-array IntVar heights)
                        capacity
                        incremental?
                        (->> filters
                             (keep filter-to-enum)
                             (into-array Cumulative$Filter)))
           ::s/invalid
           (report-spec-error constraint-name ::compile-spec var-subed-statement))))

(defn $cumulative
  "Creates a cumulative constraint:
  Enforces that at each point in time,
  the cumulated height of the set of tasks that overlap that point does not exceed a given limit.
  Task duration and height should be >= 0 Discards tasks whose duration or height is equal to zero

  filters = list-of #{:default :disjunctive-task-interval :heights
                      :nrj :sweep :sweep-hei-sort :time}

  Cumulative Filters:
  - DEFAULT Combines above filters as a black-box not idempotent
  - DISJUNCTIVE_TASK_INTERVAL energetic reasoning to filter disjunctive constraint Only propagated on variable subsets of size < 30 not idempotent not enough to ensure correctness (only an additional filtering)
  - HEIGHTS filters height variables only (sweep-based algorithm) idempotent (on the given set of variables only)
  - NRJ energetic reasoning to filter not idempotent not enough to ensure correctness (only an additional filtering)
  - SWEEP time-table algorithm based on a sweep line idempotent (on the given set of variables only)
  - SWEEP_HEI_SORT time-table algorithm based on a sweep line idempotent (on the given set of variables only)
  - TIME time-table algorithm based on each point in time not idempotent
  "
  {:choco
   ["cumulative(Task[] tasks, IntVar[] heights, IntVar capacity)"
    "cumulative(Task[] tasks, IntVar[] heights, IntVar capacity, boolean incremental)"
    "cumulative(Task[] tasks, IntVar[] heights, IntVar capacity, boolean incremental, Cumulative.Filter... filters)"]}
  ([tasks heights capacity]
   ($cumulative tasks heights capacity false))
  ([tasks heights capacity incremental?]
   ($cumulative tasks heights capacity incremental? [:default]))
  ([tasks heights capacity incremental? filters]
   {:pre
    [(sequential? filters)
     (every? filters? filters)
     (sequential? tasks)
     (sequential? heights)
     (boolean? incremental?)]}
   (constraint constraint-name
               [(vec tasks)
                ['heights (vec heights)]
                ['capacity capacity]
                ['incremental incremental?]
                ['filters (mapv (c symbol name) filters)]]
               compiler)))
