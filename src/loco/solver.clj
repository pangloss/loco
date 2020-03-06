(ns loco.solver
  (:require
   [camel-snake-kebab.core :refer [->kebab-case]]
   [meander.epsilon :as m :refer [match]]
   [loco.model :as model]
   [loco.compiler :as compiler]
   [loco.utils :refer [c p]])
  (:import
   org.chocosolver.solver.Model
   org.chocosolver.solver.Solver
   org.chocosolver.solver.Solution
   org.chocosolver.solver.ParallelPortfolio
   org.chocosolver.solver.variables.IntVar
   org.chocosolver.solver.variables.SetVar
   org.chocosolver.solver.variables.Task
   org.chocosolver.solver.variables.RealVar
   ))

(defn- problem->Model
  "creates a model from declarations from loco.constraints namespace. e.g. ($in...)"
  [problem-from-dsl]
  {:pre [(sequential? problem-from-dsl)]}
  (let [
        problem-ast (model/compile problem-from-dsl)
        ]
    ;; FIXME: setting strategy for model/solver changed since 4.0.0. look at below link
    ;; http://www.choco-solver.org/apidocs/index.html
    ;; org.chocosolver.solver.search.strategy.Search
    ;; minDomLBSearch
    #_(let [vars (vals @(:my-vars s))
            strategy (ISF/minDom_LB (into-array IntVar vars))]
        (.set (:csolver s) (into-array AbstractStrategy [strategy])))
    (compiler/compile problem-ast)))

(defn- get-compiled-model [problem]
  (let [{:keys [model? compiled?]} (meta problem)]
    (cond
      model? (->> problem compiler/compile)
      compiled? problem
      :else (->> problem model/compile compiler/compile)))
  )

(def implemented-search-monitor-methods
  (->>
   #{
     :limitBacktrack,                 ;; void <- limitBacktrack(long limit)
     :limitFail,                      ;; void <- limitFail(long limit)
     :limitNode,                      ;; void <- limitNode(long limit)
     :limitSearch,                    ;; void <- limitSearch(Criterion aStopCriterion)
     :limitSolution,                  ;; void <- limitSolution(long limit)
     :limitTime,                      ;; void <- limitTime(long limit) | (String duration)
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
    {:maximize (m/some ?var-name)} (do
                                     (.setObjective model Model/MAXIMIZE (?var-name vars-index))
                                     {:maximize ?var-name})
    {:minimize (m/some ?var-name)} (do
                                     (.setObjective model Model/MINIMIZE (?var-name vars-index))
                                     {:minimize ?var-name})
    _ nil))

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
       (reduce
        (fn [acc [var-name var]]
          (assoc! acc
                  (var-key-name-fn var-name)
                  (cond
                    (instance? Task var)
                    {
                     :start    (->> var .getStart (.getIntVal solution))
                     :duration (->> var .getDuration (.getIntVal solution))
                     :end      (->> var .getEnd (.getIntVal solution))
                     }
                    (instance? IntVar var) (.getIntVal solution var)
                    (instance? SetVar var) (set (.getSetVal solution var)))
                  ))
        (transient {}))
       persistent!))

(defn solutions
  "Solves the problem using the specified constraints and returns a map from variable names to their values (or nil if there is no solution).
  Keyword arguments:
  - :maximize <var-name> - finds the solution maximizing the given variable.
  - :minimize <var-name> - finds the solution minimizing the given variable.
  - :feasible <bool> - optimizes time by guaranteeing that the problem is feasible before trying to maximize/minimize a variable.

  available Search Monitor features:
  :limit-backtrack                      <int>
  :limit-fail                           <int>
  :limit-node                           <int>
  :limit-search                         <int>
  :limit-solution                       <int>
  :limit-time                           <int> | <string>
  :set-no-good-recording-from-restarts  <nil>
  :set-no-good-recording-from-solutions <[IntVar...]>

  Note: returned solution maps have the metadata:
  {
   :solver          <Solver>
   :search-monitors <Map>
   :model           <Model>
   :model-objective <Map>
  }"
  [problem & args]
  (if (instance? Model problem)
    ;; we were given a choco model instead of something nicely wrapped
    ;; we will still attempt to solve the model but we don't do anything fancy
    (->
     problem
     (.getSolver)
     (.streamSolutions nil) ;;lawl, doesn't work without args
     (.iterator)
     iterator-seq)

    ;;we are given a nice loco object, do nice things and return a seq of clojure stuff
    (let [args-map (apply hash-map args)
          {:keys [constraints
                  model
                  public-vars-index
                  vars-index
                  var-name-mapping]
           } (get-compiled-model problem)
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
       (->> (map solution-extractor))
       (with-meta {:solver solver
                   :search-monitors search-monitors
                   :model model
                   :model-objective model-objective})))))

(def solution (c first (p solutions)))
(alter-meta! #'solution merge (dissoc (meta #'solutions) :name))

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
         } (get-compiled-model problem)
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
     (->> (map solution-extractor))
     (with-meta {:solver solver
                 :search-monitors search-monitors
                 :model model}))))

#_(declare parallel-portfolio)
#_(declare portfolio-seq)

#_(defn parallel-solutions [problem]
    (let [;;args-map (apply hash-map args)
          {:keys [constraints
                  model
                  public-vars-index
                  vars-index
                  var-name-mapping]
           } (problem->Model problem)
          var-key-name-fn (if (empty? var-name-mapping)
                            identity
                            (memoize (fn [var-name] (get var-name-mapping var-name var-name))))
          solution-extractor (p extract-solution public-vars-index var-key-name-fn)
          ;; search-monitors (set-search-monitor-settings! solver args-map)
          ;; [optimization-type optimized-var-name] (->
          ;;                                         args-map
          ;;                                         (select-keys [:minimize :maximize])
          ;;                                         vec
          ;;                                         first)
          ;; optimization-var (optimized-var-name vars-index)
          ;; optimization-type-TF ({:maximize true :minimize false} optimization-type)

          ;;taken from pmap
          n (+ 2 (.. Runtime getRuntime availableProcessors))
          models (repeatedly n #(->> problem problem->Model))
          portfolio (parallel-portfolio models)
          ]
      (portfolio-seq portfolio models)))


;;TODO: needs better understanding in order to figure out if this will work or not
#_(defn- parallel-portfolio
    " A Portfolio helper.

  The ParallelPortfolio resolution of a problem is made of four steps:

    adding models to be run in parallel,
    running resolution in parallel,
    getting the model which finds a solution (or the best one), if any.

  Each of the four steps is needed and the order is imposed too. In
  particular, in step 1. each model should be populated individually
  with a model of the problem (presumably the same model, but not
  required). Populating model is not managed by this class and should
  be done before applying step 2., with a dedicated method for
  instance.  Note also that there should not be pending resolution
  process in any models. Otherwise, unexpected behaviors may occur.

  The resolution process is synchronized. As soon as one model
  ends (naturally or by hitting a limit) the other ones are eagerly
  stopped. Moreover, when dealing with an optimization problem, cut on
  the objective variable's value is propagated to all models on
  solution. It is essential to eagerly declare the objective
  variable(s) with Model.setObjective(boolean, Variable).

  Note that the similarity of the models declared is not
  required. However, when dealing with an optimization problem, keep
  in mind that the cut on the objective variable's value is propagated
  among all models, so different objectives may lead to wrong results.

  Since there is no condition on the similarity of the models, once
  the resolution ends, the model which finds the (best) solution is
  internally stored. "
    [models]
    ;; ParallelPortfolio pares = new ParallelPortfolio();
    ;; int n = 4; // number of models to use
    ;; for (int i = 0; i < n; i++) {
    ;;          pares.addModel(modeller());
    ;;          }
    ;; pares.solve();
    ;; IOutputFactory.printSolutions(pares.getBestModel());

    (let [portfolio (ParallelPortfolio.)]
      (doseq [model models]
        (.addModel portfolio (:model model)))
      portfolio))

(defn- extract-model
  "the user is allowed to make var-names vectors or other objects, these
  are converted to strings before the Model is created, here we remap
  those strings back to the original objects that they possibly
  are. most likely the var-key-name-fn is the identity function"
  [model]
  ;;var-map looks like:
  ;;
  ;;{:y #object[org.chocosolver.solver.variables.impl.BitsetIntVarImpl 0x56881196 y = {1..3}}
  (let [{:keys [constraints
                model
                public-vars-index
                vars-index
                var-name-mapping]
         } model

        ;;would need to assoc this to the models for memoize to work
        var-key-name-fn (if (empty? var-name-mapping)
                          identity
                          (memoize (fn [var-name] (get var-name-mapping var-name var-name))))]

    (->> public-vars-index
         (reduce (fn [acc [var-name var]]
                   (assoc acc
                          (var-key-name-fn var-name)
                          (.getValue var)))
                 {}))))

(defn- portfolio-seq [^ParallelPortfolio portfolio, models]
  (let [
        ;;search-monitors (set-search-monitor-settings! solver args-map)
        ;;model-objective (set-model-objective! model vars-index args-map)
        find-model (fn [best-model] (first
                                     (drop-while #(not= best-model (:model %)) models)))
        ]

    (when (.solve portfolio)
      (lazy-seq
       (cons
        (->>
         (.getBestModel portfolio)
         find-model
         extract-model)
        (portfolio-seq portfolio models)
        )))))
