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

(defn const-reals [model prefixes name vals]
  (mapv
   (fn [val prefix]
     (let [str-name (str prefix " " name)]
       {:gensym (gensym str-name)
        :var (.realVar model str-name, val)}))
   vals prefixes))

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

(defn ontario-tax-bracket-model [model prefix choco-income-var]
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
                      choco-income-tax-bracket-var (choco-real model [prefix "taxed-income" max] 0, max, 100.0)
                      {income-tax-bracket-gensym :gensym} choco-income-tax-bracket-var
                      choco-income-tax-payment-var (choco-real model [prefix "taxed-amount %" (int (* 100 percent))] 0, (* max percent), 100.0)
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
    model
    ))

(def years
  [2045
   2046 2047 2048 2049 2050 2051 2052 2053 2054 2055 2056 2057 2058 2059 2060 2061 2062 2063 2064 2065 2066 2067 2068
   2069 2070 2071 2072 2073 2074 2075 2076 2077 2078 2079 2080 2081 2082 2083 2084 2085 2086 2087 2088 2089 2090 2091
   2092 2093 2094 2095 2096 2097 2098 2099 2100 2101 2102 2103 2104 2105 2106 2107 2108 2109 2110 2111 2112 2113
   ])

(def incomes
  [107745.45 62869.16 75585.43 76780.60 66258.49 67499.16 68763.24 84076.29 86050.84 88087.16 90283.00 91481.67 92699.00
   93928.89 95164.98 96400.72 97732.21 98988.52 100316.52 101647.03 102965.19 104346.23 105715.42 107091.02 108521.24
   109917.03 111346.24 112807.78 114276.33])

;; This only barely works if Ibex can be initalized, if we don't have ibex we shouldn't run this... dono how to determin that yet, however ibex is really really slow.

#_(->>
   (let [
         precision 100.0
         model (new Model)
         income-vars (const-reals model years "net-income" (take 1 incomes))
         _model (doall
                 (map (fn [income-var year]
                        (ontario-tax-bracket-model model year income-var))
                      income-vars years))
         ]
     (-> model (.setPrecision precision))
     ;;(-> model (.setObjective Model/MAXIMIZE z))
     [
      "-------------------- model"
      model
      "-------------------- solutions"
      ((juxt count identity) (time (solver/solutions model)))
      ]
     )
   println
   )
