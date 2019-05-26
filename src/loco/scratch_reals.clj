(ns loco.scratch-reals
  (:use loco.utils)
  (:require
   [loco.solver :as solver]
   )
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

(defn cyclo-hexan-problem-model [precision]
  (let [model (new Model)
        x (.realVar model "x", ##-Inf ##Inf precision)
        y (.realVar model "y", -1.0e8, 1.0e8, precision)
        z (.realVar model "z", -1.0e8, 1.0e8, precision)

        constraint-str (str
                        "{1}^2 * (1 + {2}^2) + {2} * ({2} - 24 * {1}) = -13;"
                        "{0}^2 * (1 + {1}^2) + {1} * ({1} - 24 * {0}) = -13;"
                        "{2}^2 * (1 + {0}^2) + {0} * ({0} - 24 * {2}) = -13")
        vars (into-array RealVar [x y z])
        ]
    (->
     model
     (.realIbexGenericConstraint constraint-str vars)
     (.post))
    (-> model (.setPrecision precision))
    {
     :x x
     :y y
     :z z
     :model model
     }
    )
  )

(let [
      precision 1.0e-4
      ;;precision 1.0
      {normal :model} (cyclo-hexan-problem-model precision)
      {max :model x :x y :y z :z} (cyclo-hexan-problem-model precision)
      ]
  (-> max (.setObjective Model/MAXIMIZE z))
  [
   normal
   max
   ["--------------------max"]
   (time (doall (solver/solutions max)))
   ;;((juxt count identity) (time (solver/solutions max)))
   ["--------------------reg"]
   (time (doall (solver/solutions normal)))
   ;;((juxt count identity) (time (solver/solutions normal)))
   ]
  )
