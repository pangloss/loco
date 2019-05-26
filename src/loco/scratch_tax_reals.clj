(ns loco.scratch-tax-reals
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

(def tax-bracket-amounts
  (mapv int [1000000.00, 220000.00, 210371.00, 150000.00, 147667.00, 95259.00, 91101.00, 87813.00, 77313.00, 47630.00, 43906.00]))

(def tax-bracket-percents
  (map (p * 0.01) [53.53, 51.97, 47.97, 46.41, 43.41, 37.91, 33.89, 31.48, 29.65, 24.15, 20.05]))

(def tax-bracket-ranges
  (reverse
   (map vector
        (rest (conj tax-bracket-amounts 0))
        tax-bracket-amounts
        tax-bracket-percents)))

#_(def tax-bracket-pairs
    (map vector
         tax-bracket-amounts
         tax-bracket-percents))

;;=MAX(0,MIN($AM104,BA$101)-BB$101)
;;AM104 = net income
;;BA101 = current tax bracket amount (cumulative/const)
;;BB101 = last tax bracket amount (cumulative/const)

(defn const-real [model name val]
  (let [str-name (str name)]
    {:gensym (gensym str-name)
     :var (.realVar model str-name, val)}))

(defn choco-real [model name lb ub precision]
  (let [str-name (str name)]
    {:gensym (gensym str-name)
     :var (.realVar model str-name, lb, ub, precision)}))

(defn choco-constraint [constraint-vec]
  {:constraint constraint-vec})


(defn magic [choco-vars choco-constraints]
  ;; the constraint structure for ibex should be in the form
  ;; ["ibex math" gensym "ibex math" gensym"]
  ;; this way we can map the gensyms to the position of the vars, rewrite the gensyms as those positions when we need to.
  (let [replace-mapping (->> choco-vars flatten (map :gensym) (map-indexed #(vector %2 (str "{" %1 "}"))) (into {}))
        vars            (->> choco-vars flatten (map :var))
        constraints     (->> choco-constraints
                             flatten
                             (map :constraint)
                             (map (fn [co] (replace replace-mapping co)))
                             (map (fn [co] (conj co ";")))
                             (map (p apply str))
                             vec)]
    {:vars vars
     :constraints constraints}))

(defn ontario-tax-bracket-model [model choco-income-var precision]
  ;;bracket-amount=MAX(0,MIN(net-income,current-tax-bracket)-previous-tax-bracket)
  ;; 0 1 2 3 [bracket-amount net-income current-tax-bracket previous-tax-bracket]
  ;;(str "max(0, min({0}, {1}) - {2}) = {3};")
  (let [
        {income-gensym :gensym income-var :var} choco-income-var
        taxed-incomes
        (->> tax-bracket-ranges
             (map
              (fn [[min max percent]]
                (let [
                      choco-income-tax-bracket-var (choco-real model ["taxed-income" max] 0, max, 1.0)
                      {income-tax-bracket-gensym :gensym} choco-income-tax-bracket-var
                      choco-income-tax-payment-var (choco-real model ["taxed-amount %" (int (* 100 percent))] 0, (* max percent), 0.01)
                      {income-tax-payment-gensym :gensym} choco-income-tax-payment-var
                      ]
                  {
                   :vars [choco-income-tax-bracket-var
                          choco-income-tax-payment-var]
                   :constraints
                   [
                    (choco-constraint ["max(0, min( " income-gensym " , " max " ) - " min " ) = " income-tax-bracket-gensym])
                    (choco-constraint [income-tax-bracket-gensym " * " percent " = " income-tax-payment-gensym])
                    ]
                   }))))
        {tax-vars :vars
         tax-constraint-strs :constraints} (magic [choco-income-var, (map :vars taxed-incomes)]
                                              (map :constraints taxed-incomes))
        tax-bracket-constraint-str (apply str tax-constraint-strs)
        vars (into-array RealVar tax-vars)
        ]
    (->
     model
     (.realIbexGenericConstraint tax-bracket-constraint-str vars)
     (.post))
    (-> model (.setPrecision precision))
    model
    ))

(->>
 (let [
       precision 1.0
       model (new Model)
       income-var (const-real model "income" 107745.45)
       model (ontario-tax-bracket-model model income-var precision)
       ]
   ;;(-> model (.setObjective Model/MAXIMIZE z))
   [
    model
    ["--------------------reg"]
    (time (doall (solver/solutions model)))
    ]
   )
 println
 )
