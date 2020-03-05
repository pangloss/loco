(ns loco.constraints.cumulative
  (:require
   [meander.epsilon :as m :refer [match]]
   [clojure.spec.alpha :as s]
   [clojure.walk :as walk]
   [loco.constraints.utils :refer :all :as utils]
   )
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
                       ::utils/coll-taskvar?
                       (s/tuple #{'heights}     ::utils/coll-coerce-intvar?)
                       (s/tuple #{'capacity}    ::utils/coerce-intvar?)
                       (s/tuple #{'incremental} boolean?)
                       (s/tuple #{'filters}     (s/coll-of filters?))
                       ))))

(compile-function
 (let [coerce-var (utils/coerce-var *model)]
   (match *conformed
     {:args [?tasks [_ ?heights] [_ ?capacity] [_ ?incremental?] [_ ?filters]]}
     (.cumulative *model
                  (into-array Task ?tasks)
                  (->> ?heights (map coerce-var) (into-array IntVar))
                  (coerce-var ?capacity)
                  ?incremental?
                  (->> ?filters
                       (keep filter-to-enum)
                       (into-array Cumulative$Filter))))))

;;TODO: create args-list for $cumulative
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
    "cumulative(Task[] tasks, IntVar[] heights, IntVar capacity, boolean incremental, Cumulative.Filter... filters)"]
   :gccat "http://sofdem.github.io/gccat/gccat/Ccumulative.html"}
  ([tasks & {:keys [heights capacity incremental? filters]}]
   {:pre
    [(or (sequential? filters) (nil? filters))
     (every? filters? filters)
     (or (sequential? tasks) (map? tasks))
     (or (sequential? heights) (nil? heights))
     (or (boolean? incremental?) (nil? incremental?))]}
   (match
    [tasks heights capacity incremental? filters]

    [(m/pred map? ?tasks) nil ?capacity ?incremental? ?filters]
    ($cumulative (keys ?tasks) :heights (vals ?tasks) :capacity ?capacity :incremental? ?incremental? :filters ?filters)

    [(m/pred sequential? ?tasks) ?heights ?capacity ?incremental? ?filters]
    (constraint constraint-name
                [(vec ?tasks)
                 ['heights (vec ?heights)]
                 ['capacity ?capacity]
                 ['incremental ?incremental?]
                 ['filters (mapv (comp symbol name) ?filters)]]
                compiler))))
