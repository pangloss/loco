(ns loco.constraints.table
  (:require
   [clojure.spec.alpha :as s]
   [loco.constraints.utils :refer :all :as utils]
   [loco.match :refer [match+]]
   [clojure.core.match :refer [match]]
   [clojure.walk :as walk])
  (:import
   [org.chocosolver.solver.variables SetVar IntVar BoolVar Task]))

(def ^:private constraint-name 'table)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/or
                       :vars-tuples      (s/tuple
                                          (s/coll-of int-var?)
                                          (s/tuple #{'tuples}  tuples-var?))

                       :vars-tuples-algo  (s/tuple
                                          (s/coll-of int-var?)
                                          (s/tuple #{'tuples}  tuples-var?)
                                          (s/tuple #{'algo} #{'CT+ 'GAC2001 'GAC2001+
                                                              'GAC3rm 'GAC3rm+ 'GACSTR+
                                                              'STR2+ 'FC 'MDD+}))

                       :pair-tuples      (s/tuple
                                          (s/tuple #{'pair} int-var? int-var?)
                                          (s/tuple #{'tuples} tuples-var?))

                       :pair-tuples-algo (s/tuple
                                          (s/tuple #{'pair} int-var? int-var?)
                                          (s/tuple #{'tuples} tuples-var?)
                                          (s/tuple #{'algo} #{'AC2001 'AC3 'AC3rm
                                                              'AC3bit+rm 'FC}))))))

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:args [:vars-tuples [vars [_ tuples]]]}
           (.table model (into-array IntVar vars) tuples)

           {:args [:vars-tuples-algo [vars [_ tuples] [_ algo]]]}
           (.table model (into-array IntVar vars) tuples (name algo))

           {:args [:pair-tuples [[_ var1 var2] [_ tuples]]]}
           (.table model var1 var2 tuples)

           {:args [:pair-tuples-algo [[_ var1 var2] [_ tuples] [_ algo]]]}
           (.table model var1 var2 tuples (name algo))

           ::s/invalid
           (report-spec-error constraint-name ::compile-spec var-subed-statement))))

(defloco $table
  "-------------------- IntVar [] --------------------
  ;; table(IntVar[] vars, Tuples tuples)
  ;; table(IntVar[] vars, Tuples tuples, String algo)

  Creates a table constraint specifying that the sequence of variables
  vars must belong to the list of tuples (or must NOT belong in case
  of infeasible tuples) Default configuration with GACSTR+ algorithm
  for feasible tuples and GAC3rm otherwise

  algo = of #{:CT+ :GAC2001 :GAC2001+ :GAC3rm :GAC3rm+ :GACSTR+ :STR2+ :FC :MDD+}

  Creates a table constraint, with the specified algorithm defined algo

  - CT+: Compact-Table algorithm (AC),
  - GAC2001: Arc Consistency version 2001 for tuples,
  - GAC2001+: Arc Consistency version 2001 for allowed tuples,
  - GAC3rm: Arc Consistency version AC3 rm for tuples,
  - GAC3rm+ (default): Arc Consistency version 3rm for allowed tuples,
  - GACSTR+: Arc Consistency version STR for allowed tuples,
  - STR2+: Arc Consistency version STR2 for allowed tuples,
  - FC: Forward Checking.
  - MDD+: uses a multi-valued decision diagram for allowed tuples (see mddc constraint),


  -------------------- IntVar pair --------------------

  ;; table(IntVar var1, IntVar var2, Tuples tuples)
  ;; table(IntVar var1, IntVar var2, Tuples tuples, String algo)

  algo = of #{:AC2001 :AC3 :AC3rm :AC3bit+rm :FC}

  Creates a table constraint over a couple of variables var1 and var2:
  - AC2001: table constraint which applies the AC2001 algorithm,
  - AC3: table constraint which applies the AC3 algorithm,
  - AC3rm: table constraint which applies the AC3 rm algorithm,
  - AC3bit+rm (default): table constraint which applies the AC3 bit+rm algorithm,
  - FC: table constraint which applies forward checking algorithm."

  {:choco ["table(IntVar[] vars, Tuples tuples)"
           "table(IntVar[] vars, Tuples tuples, String algo)"
           "table(IntVar var1, IntVar var2, Tuples tuples)"
           "table(IntVar var1, IntVar var2, Tuples tuples, String algo)"
           ]
   :arglists '([int-vars tuples]
               [int-vars tuples algo]
               [int-var1 int-var2 tuples]
               [int-var1 int-var2 tuples algo])
   }
  [& more]
  (match+
   (vec more)
   [int-vars tuples] :guard [int-vars sequential?]
   (constraint constraint-name
               [(vec int-vars)
                ['tuples tuples]]
               compiler)

   [int-vars tuples algo]
   :guard [int-vars sequential?
           ;;TODO: symbolize and stringize the algos for $table
           algo #{:CT+ :GAC2001 :GAC2001+ :GAC3rm :GAC3rm+ :GACSTR+ :STR2+ :FC :MDD+}]
   (constraint constraint-name
               [(vec int-vars)
                ['tuples tuples]
                ['algo (symbol (name algo))]]
               compiler)

   [int-var1 int-var2 tuples] :guard [[int-var1 int-var2] keyword?]
   (constraint constraint-name
               [['pair int-var1 int-var2]
                ['tuples tuples]]
               compiler)

   [int-var1 int-var2 tuples algo]
   :guard [[int-var1 int-var2] keyword?
           algo #{:AC2001 :AC3 :AC3rm :AC3bit+rm :FC}]
   (constraint constraint-name
               [['pair int-var1 int-var2]
                ['tuples tuples]
                ['algo (symbol (name algo))]]
               compiler)))
