(ns loco.solver
  (:use loco.utils)
  (:require
   [camel-snake-kebab.core :refer [->kebab-case]]
   [clojure.core.match :refer [match]]
   [loco.model :as model]
   [loco.compiler :as compiler])
  (:import
   org.chocosolver.solver.Model
   org.chocosolver.solver.Solver
   org.chocosolver.solver.Solution))

(defn problem->solver
  "creates a model from declarations from loco.constraints namespace. e.g. ($in...)"
  [problem-from-dsl]
  {:pre [(coll? problem-from-dsl)]}
  (let [
        problem-ast (model/compile problem-from-dsl)
        ]
    ;; FIXME: things changed in 4.0.0
    ;; http://www.choco-solver.org/apidocs/index.html
    ;; org.chocosolver.solver.search.strategy.Search
    ;; minDomLBSearch
    #_(let [vars (vals @(:my-vars s))
            strategy (ISF/minDom_LB (into-array IntVar vars))]
        (.set (:csolver s) (into-array AbstractStrategy [strategy])))
    (compiler/compile problem-ast)))

(def implemented-search-monitor-methods
  (->>
   #{
     :limitBacktrack, ;; void <- limitBacktrack(long limit)
     :limitFail, ;; void <- limitFail(long limit)
     :limitNode, ;; void <- limitNode(long limit)
     :limitSearch, ;; void <- limitSearch(Criterion aStopCriterion)
     :limitSolution, ;; void <- limitSolution(long limit)
     :limitTime, ;; void <- limitTime(long limit) | (String duration)
     :setNoGoodRecordingFromRestarts, ;; void <- setNoGoodRecordingFromRestarts()
     :setNoGoodRecordingFromSolutions ;; void <- setNoGoodRecordingFromSolutions(IntVar... vars)
     }
   (map (c (juxt ->kebab-case (c symbol name))))
   (into (sorted-map))))

(defn set-search-monitor-settings! [solver named-params]
  (doseq [[method-name arg] named-params]
    ;;only execute methods in the scope of SearchMonitor
    (when-let [method (method-name implemented-search-monitor-methods)]
        (if (nil? arg)
          (. solver method)
          (. solver method arg)))))

(defn set-model-objective! [model vars-index named-params]
  (match named-params
         {:maximize var-name} (.setObjective model Model/MAXIMIZE (var-name vars-index))
         {:minimize var-name} (.setObjective model Model/MINIMIZE (var-name vars-index))
         {} nil))

(defn extract-solution [var-map, ^Solution solution]
  ;;var-map looks like:
  ;;
  ;;[[:var :y :public [:int 1 3]] #object[org.chocosolver.solver.variables.impl.BitsetIntVarImpl 0x56881196 y = {1..3}]]
  (->> var-map
       (map (fn [[[_ var-name _ _] var]]
              [var-name (.getIntVal solution var)]))
       (into {})))

(defn solutions
  "Solves the problem using the specified constraints and returns a map from variable names to their values (or nil if there is no solution).
  Keyword arguments:
  - :maximize <var> - finds the solution maximizing the given variable.
  - :minimize <var> - finds the solution minimizing the given variable.
  - :feasible true - optimizes time by guaranteeing that the problem is feasible before trying to maximize/minimize a variable.
  - :timeout <number> - stops after a certain amount of milliseconds (returns nil, or best solution so far when min/maxing a variable)
  Note: returned solution maps have the metadata {:loco/solution <n>} denoting that it is the nth solution found (starting with 0)."
  [problem & args]
  (let [args-map (apply hash-map args)
        {
         constraints :constraints
         model :model
         vars :public-vars-map
         vars-index :vars-index
         } (problem->solver problem)
        solver      (.getSolver model)
        solution-extractor (p extract-solution vars)
        ]
    ;;(set-search-monitor-settings! solver args-map)
    (set-model-objective! model vars-index args-map)
    (->
     solver
     (.streamSolutions nil) ;;lawl
     (.iterator)
     iterator-seq
     (->>
      (map solution-extractor)))))

(defn solution
  "Solves the problem using the specified constraints and returns a map from variable names to their values (or nil if there is no solution).
  Keyword arguments:
  - :maximize <var> - finds the solution maximizing the given variable.
  - :minimize <var> - finds the solution minimizing the given variable.
  - :feasible true - optimizes time by guaranteeing that the problem is feasible before trying to maximize/minimize a variable.
  - :timeout <number> - stops after a certain amount of milliseconds (returns nil, or best solution so far when min/maxing a variable)
  Note: returned solution maps have the metadata {:loco/solution <n>} denoting that it is the nth solution found (starting with 0)."
  [problem & args]
  ;;setup solution
  ;;to maximize X
  ;;model.setObjective(Model.MAXIMIZE, X);
  ;;or model.setObjective(Model.MINIMIZE, X); to minimize X
  ;;{:pre [(even? (count args))]}
  (->> (apply solutions problem args)
       first))

;;converting java steam to clojure seq
#_(-> stream
    .iterator
    iterator-seq)
