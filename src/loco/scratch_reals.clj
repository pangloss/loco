(ns loco.scratch-reals
  (:require
     [loco.solver :as solver])
  (:import
   (org.chocosolver.solver.constraints.real Ibex)
   ;; (org.chocosolver.solver.variables IntVar)
   ;; (org.chocosolver.solver.exception SolverException)
   ;; org.chocosolver.solver.variables.Variable
   ;; (org.chocosolver.solver
   ;;  Model
   ;;  ParallelPortfolio
   ;;  ResolutionPolicy
   ;;  Solution
   ;;  Solver)
   ;; org.chocosolver.solver.constraints.Constraint
   ;; (org.chocosolver.solver.search.loop.monitors ISearchMonitorFactory)
   ;; (org.chocosolver.util ESat)
   ;;org.chocosolver.samples AbstractProblem
   (org.chocosolver.solver Model)
   (org.chocosolver.solver Solver)
   (org.chocosolver.solver.search.loop.monitors IMonitorSolution)
   (org.chocosolver.solver.variables RealVar)
   (org.chocosolver.solver.search.strategy Search) ;;Search/realVarSearch
   )
  )

;; from: /ibex/choco_samples/src/main/java/org/chocosolver/samples/real/

;; negative infinity
;;##-Inf

;; positive infinity
;;##Inf

(->
 (let [model (new Model)
       precision 1.0e-6
       precision 1.0e-2
       x (.realVar model "x", ##-Inf ##Inf precision)
       y (.realVar model "y", -1.0e8, 1.0e8, precision)
       z (.realVar model "z", -1.0e8, 1.0e8, precision)

       constraint-str (str
                       "{1}^2 * (1 + {2}^2) + {2} * ({2} - 24 * {1}) = -13;"
                       "{0}^2 * (1 + {1}^2) + {1} * ({1} - 24 * {0}) = -13;"
                       "{2}^2 * (1 + {0}^2) + {0} * ({0} - 24 * {2}) = -13")
       ;;vars = new RealVar[]{x, y, z};
       vars (into-array RealVar [x y z])
       ]
   ;; model.realIbexGenericConstraint(
   ;;                                 "{1}^2 * (1 + {2}^2) + {2} * ({2} - 24 * {1}) = -13;" +
   ;;                                 "{0}^2 * (1 + {1}^2) + {1} * ({1} - 24 * {0}) = -13;" +
   ;;                                 "{2}^2 * (1 + {0}^2) + {0} * ({0} - 24 * {2}) = -13",
   ;;                                 vars).post();

   ;; mutation happens here! yay!
   (->
    model
    (.realIbexGenericConstraint constraint-str vars)
    (.post))

   [
    ;;model
    (instance? org.chocosolver.solver.search.IResolutionHelper (.getSolver model))
    (instance? org.chocosolver.solver.ISolver (.getSolver model))
    ;;(.streamSolutions (.getSolver model))
    ;; (.solve (.getSolver model))
    ;; (.solve (.getSolver model))
    (.streamSolutions (.getSolver model) nil)
    (time (solver/solution model))
    (time (solver/solutions model))
    ]

   )
 println
 )
