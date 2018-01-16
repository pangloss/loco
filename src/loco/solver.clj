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

(defn- problem->solver
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

(defn- set-search-monitor-settings! [solver named-params]
  (->>
   named-params
   (keep (fn [[method-name args]]
           (when-let [method (method-name implemented-search-monitor-methods)]
             `(. ~solver ~method ~@args)
             {method-name args})))
   (into {})
   doall))

(defn- set-model-objective! [model vars-index named-params]
  (match named-params
         {:maximize var-name} (do
                                (.setObjective model Model/MAXIMIZE (var-name vars-index))
                                {:maximize var-name})
         {:minimize var-name} (do
                                (.setObjective model Model/MINIMIZE (var-name vars-index))
                                {:minimize var-name})
         {} nil))

(defn- extract-solution
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
  - :maximize <var-name> - finds the solution maximizing the given variable.
  - :minimize <var-name> - finds the solution minimizing the given variable.
  - :feasible <bool> - optimizes time by guaranteeing that the problem is feasible before trying to maximize/minimize a variable.

  available Search Monitor features:
  :limit-backtrack <int>
  :limit-fail <int>
  :limit-node <int>
  :limit-search <int>
  :limit-solution <int>
  :limit-time <int> | <string>
  :set-no-good-recording-from-restarts <nil>
  :set-no-good-recording-from-solutions <[IntVar...]>

  Note: returned solution maps have the metadata:
  {
   :solver <Solver>
   :search-monitors <Map>
   :model <Model>
   :model-objective <Map>
  }"
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
     (.streamSolutions nil) ;;lawl, doesn't work without args
     (.iterator)
     iterator-seq
     (->>
      (map solution-extractor))
     (with-meta {:solver solver
                 :search-monitors search-monitors
                 :model model
                 :model-objective model-objective}))))

(def solution (c first (p solutions)))
(alter-meta! (var solution) merge (select-keys (meta (var solutions)) [:doc :arglists]))

(defn optimal-solutions
  "Like solutions, however requires that :maximize or :minimize is
  present in keyword args. returns sequence of all found optimal
  solutions for that variable"
  [problem & args]
  {:pre [(->> args (filter #{:minimize :maximize}) count (p = 1))]}
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
        [optimization-type optimized-var-name] (->
                                                args-map
                                                (select-keys [:minimize :maximize])
                                                vec
                                                first)
        optimization-var (optimized-var-name vars-index)
        optimization-type-TF ({:maximize true :minimize false} optimization-type)
        ]


    (->
     solver
     (.streamOptimalSolutions optimization-var optimization-type-TF nil)
     (.iterator)
     iterator-seq
     (->>
      (map solution-extractor))
     (with-meta {:solver solver
                 :search-monitors search-monitors
                 :model model}))))
