(ns loco.solver.solutions
  (:use clojure.test
        loco.constraints)
  (:require
   [loco.solver :as solver]
   [loco.model :as model]))

(def model [($const :a 1)
            ($const :b 2)
            ($int :z [1 2 3])
            ($int :y [4 5 6])
            ])

(defmacro assert-objective [expected model & args]
  `(is (= ~expected
          (->
           ~model
           (solver/solutions ~@args)
           meta
           ((juxt (comp str (memfn getObjective) :model)
                   :model-objective))))))

(defmacro assert-search-monitors [])

(deftest model-objective-tests
  (assert-objective ["a = 1" {:maximize :a}] model :maximize :a)
  (assert-objective ["a = 1" {:minimize :a}] model :minimize :a)
  (assert-objective ["b = 2" {:maximize :b}] model :maximize :b)
  (assert-objective ["b = 2" {:minimize :b}] model :minimize :b)
  (assert-objective ["" nil] model)
  )

(deftest search-monitor-tests
  ;;TODO: implement search-monitors [limitSearch, setNogoodrecordingfromsolutions]
  ;;:limitSearch, ;; void <- limitSearch(Criterion aStopCriterion)
  ;;:setNoGoodRecordingFromSolutions ;; void <- setNoGoodRecordingFromSolutions(IntVar... vars)
  (def search-monitor-params
    [
     [:limit-backtrack 10] ;; void <- limitBacktrack(long limit)
     [:limit-fail 10] ;; void <- limitFail(long limit)
     [:limit-node 10] ;; void <- limitNode(long limit)
     [:limit-solution 10] ;; void <- limitSolution(long limit)
     [:limit-time 10] ;; void <- limitTime(long limit) | (String duration)
     [:limit-time "10s"] ;; void <- limitTime(long limit) | (String duration)
     [:set-no-good-recording-from-restarts nil] ;; void <- setNoGoodRecordingFromRestarts()
     ])

  (doseq [[param-name args] search-monitor-params]
    (is
     (=
      (hash-map param-name args)
      (-> model (solver/solutions param-name args) meta :search-monitors))
     (str "should accept search monitor params " param-name " = " args)))

  (let [param-name :not-implemented
        args nil]
    (is
     (empty?
      (-> model (solver/solutions param-name args) meta :search-monitors))
     (str "should not execute unimplemented methods")))

  (is
   (=
    {:limit-backtrack 10,
     :limit-fail 10,
     :limit-node 10,
     :limit-solution 10,
     :limit-time "10s",
     :set-no-good-recording-from-restarts nil}
    (->>
     (flatten search-monitor-params)
     (apply solver/solutions model)
     meta
     :search-monitors))
   "should include all executed search monitors in meta")
  )

(deftest solutions-objective-test
  (is
   (=
    '({:a 1, :b 2, :z 1, :y 6})
    (solver/solutions model :maximize :y)))

  (is
   (=
    '({:a 1, :b 2, :z 1, :y 4})
    (solver/solutions model :minimize :y)))
  )

(deftest optimal-solutions-test
  (is
   (=
    '({:a 1, :b 2, :z 1, :y 6}
      {:a 1, :b 2, :z 2, :y 6}
      {:a 1, :b 2, :z 3, :y 6})
    (solver/optimal-solutions model :maximize :y)))

  (is
   (=
    '({:a 1, :b 2, :z 1, :y 4}
      {:a 1, :b 2, :z 2, :y 4}
      {:a 1, :b 2, :z 3, :y 4})
    (solver/optimal-solutions model :minimize :y)))
  )

;;FIXME: solver/parallel-solutions is currently disabled and requires
;;research and collaboration with the choco team
#_(deftest parallel-solutions-test
  (->> (solver/parallel-solutions model)
       (take 10))

  )
