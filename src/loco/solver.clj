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
  (->>
   named-params
   (keep (fn [[method-name args]]
           (when-let [method (method-name implemented-search-monitor-methods)]
             `(. ~solver ~method ~@args)
             {method-name args})))
   (into {})
   doall))

(defn set-model-objective! [model vars-index named-params]
  (match named-params
         {:maximize var-name} (do
                                (.setObjective model Model/MAXIMIZE (var-name vars-index))
                                {:maximize var-name})
         {:minimize var-name} (do
                                (.setObjective model Model/MINIMIZE (var-name vars-index))
                                {:minimize var-name})
         {} nil))

(defn extract-solution
  "the user is allowed to make var-names vectors or other objects, these
  are converted to strings before the Model is created, here we remap
  those strings back to the original objects that they possibly
  are. most likely the var-key-name-fn is the identity function"
  [var-map, var-key-name-fn, ^Solution solution]
  ;;var-map looks like:
  ;;
  ;;{:y #object[org.chocosolver.solver.variables.impl.BitsetIntVarImpl 0x56881196 y = {1..3}}
  (->> var-map
       (reduce (fn [acc [var-name var]]
                 (assoc acc
                        (var-key-name-fn var-name)
                        (.getIntVal solution var))) {})))

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
        {:keys [constraints
                model
                public-vars-index
                vars-index
                var-name-mapping]
         } (problem->solver problem)
        solver (.getSolver model)
        var-key-name-fn (if (empty? var-name-mapping)
                          identity
                          (memoize (fn [var-name] (get var-name-mapping var-name var-name))))
        solution-extractor (p extract-solution public-vars-index var-key-name-fn)
        search-monitors (set-search-monitor-settings! solver args-map)
        model-objective (set-model-objective! model vars-index args-map)
        ]


    (->
     solver
     (.streamSolutions nil) ;;lawl
     (.iterator)
     iterator-seq
     (->>
      (map solution-extractor))
     (with-meta {:solver solver
                 :search-monitors search-monitors
                 :model model
                 :model-objective model-objective}))))

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
