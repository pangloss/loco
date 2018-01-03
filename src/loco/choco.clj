(ns loco.choco
  loco.core :as core :refer [->choco ->choco* *solver*]
  (:import (org.chocosolver.solver.constraints
            Arithmetic
            Constraint)

           org.chocosolver.solver.Model

           (org.chocosolver.solver.variables
            IntVar
            BoolVar)

           org.chocosolver.solver.constraints.nary.automata.FA.FiniteAutomaton))


(defn- domain-min
  [x]
  (if (number? x)
    x
    (.getLB x)))

(defn- domain-max
  [x]
  (if (number? x)
    x
    (.getUB x)))

(defn- keypoints
  [vars op neutral]
  (if (empty? vars)
    [neutral]
    (let [v (first vars)
          lo (domain-min v)
          hi (domain-max v)]
      (for [arg1 [lo hi]
            arg2 (keypoints (rest vars) op neutral)]
        (op arg1 arg2)))))

(defn- make-const-var
  [n]
  ;; may be able to remove this, many of the Choco APIs can work with Ints as constants
  (let [model (:csolver *solver*)]
    (.intVar model n)))

(defn- make-int-var
  "Quick helper function to generate an integer variable for intermediate calculations."
  [min max]
  (let [model (:csolver *solver*)]
    (.intVar model (str (gensym "_int-var")) min max)))

(defn- make-bool-var
  "Quick helper function to generate a boolean variable for intermediate calculations."
  []
  (let [model  (:csolver *solver*)]
    (.boolVar model (str (gensym "_bool-var")))))

(defn- constrain!
  [constraint]
  (let [model (:csolver *solver*)]
    (.post model constraint)))

(defn- ->int-var
  "Takes a Choco IntVar or a number, and if it is a number, coerces it to an IntVar."
  [x]
  (cond
    (number? x) (make-const-var x)
    (instance? IntVar x) x
    (instance? Constraint x) (throw (IllegalArgumentException. "Expected a variable or a number, but got a constraint"))
    :else (throw (IllegalArgumentException. "Expected an int variable or number"))))

(defn- ->choco-int-var
  "Chains ->choco and ->int-var"
  [x]
  (-> x ->choco ->int-var))

(defmethod ->choco* :int-domain
  [{var-name :name domain :domain}]
  (let [v (->choco var-name)
        model (:csolver *solver*)]
    (if (map? domain)
      (.member model v (:min domain) (:max domain))
      (.member model v (int-array (sort (distinct domain)))))))

(defmethod ->choco* :int-var [data]
  (let [{:keys [domain real-name name]} data
        model (:csolver *solver*)
        vars (:my-vars *solver*)
        bounded? (boolean (:bounded domain))
        v (case [bounded? (sequential? domain)]
            [false false] (.intVar model real-name (:min domain) (:max domain))
            [false true]  (.intVar model real-name (int-array (sort domain)))
            [true false]  (.intVar model real-name (:min domain) (:max domain) bounded?))]
    (swap! vars assoc name v)
    nil))

(defmethod ->choco* :bool-var
  [{var-name :name real-name :real-name}]
  (let [
        model (:csolver *solver*)
        v (.boolVar model real-name)]
    (swap! (:my-vars *solver*) assoc var-name v)
    nil))

;;;;; ARITHMETIC

(defn- $+view
  [x const]
  (let [model (:csolver *solver*)]
    (.intOffsetView model x const)))

(defn sum
  [model sum-list op sum-value]
  {:pre [(#{"=", "!=", ">","<",">=","<="} op)
         (or (number? sum-value)
             (instance? IntVar sum-value))]}
  (.sum model
        sum-list
        op
        sum-value))

;;FIXME: need to handle BoolVar lists
(defmethod ->choco* [:+ :=]
  [{:keys [args eq-arg] :as m}]
  (let [model (:csolver *solver*)]
    (sum model
         ;; FIXME: there is a difference for IntVars and BoolVars that needs to be considered
          (into-array IntVar (map ->choco-int-var args))
          "="
          (if (number? eq-arg)
            eq-arg
            (->choco-int-var eq-arg)))))

;;TODO: not really sure how this is used...
(defmethod ->choco* :+
  [{:keys [args] :as m}]
  (let [[x y & more :as vars] (map ->choco args)
        model (:csolver *solver*)]
    (cond
      (and (empty? more) (number? y)) ($+view x y)
      (and (empty? more) (number? x)) ($+view y x)
      :else (let [vars (map ->int-var vars) ; converting numbers to int-views
                  mins (map #(.getLB ^IntVar %) vars)
                  maxes (map #(.getUB ^IntVar %) vars)
                  sum-var (make-int-var (apply + mins) (apply + maxes))
                  vars (try (into-array BoolVar vars)
                            (catch Exception e
                              (into-array IntVar vars)))]
              (constrain! (sum model vars "=" sum-var))
              sum-var))))

(defmethod ->choco* [:- :=]
  [{:keys [args eq-arg] :as m}]
  (let [model (:csolver *solver*)
        first-arg (->> args first ->choco-int-var)
        rest-args (->> args rest (map ->choco-int-var) (map #(.intMinusView model %)))]
    (sum
     model
     (->> (concat [first-arg] rest-args)
          (into-array IntVar))
     "="
     (if (number? eq-arg)
       eq-arg
       (->choco-int-var eq-arg)))))

(defmethod ->choco* :neg
  [{x :arg}]
  (let [x (->choco x)
        model (:csolver *solver*)]
    (if (number? x)
      (- x)
      (.intMinusView model x))))

(defn- $*view
  [x const]
  (let [model (:csolver *solver*)]
    (.intScaleView model x const)))

(defmethod ->choco* [:* :=]
  [{x :arg1 y :arg2 z :eq-arg}]
  (let [x (->choco x)
        y (->choco y)
        z (->choco-int-var z)
        model (:csolver *solver*)]
    (cond
      (number? y) (.arithm model ($*view x y) "=" z)
      (number? x) (.arithm model ($*view y x) "=" z)
      :else (.times model x y z))))

(defmethod ->choco* :*
  [{x :arg1 y :arg2}]
  (let [x (->choco x)
        y (->choco y)
        model (:csolver *solver*)]
    (cond
      (number? y) ($*view x y)
      (number? x) ($*view y x)
      :else (let [nums (keypoints [x y] * 1)
                  total-min (apply min nums)
                  total-max (apply max nums)
                  z (make-int-var total-min total-max)]
              (constrain! (.times model x y z))
              z))))


(defmethod ->choco* [:min :=]
  [{:keys [args eq-arg]}]
  (let [args (map ->choco-int-var args)
        eq-arg (->choco-int-var eq-arg)
        model (:csolver *solver*)]
    (if (= (count args) 2)
      (.min model eq-arg (first args) (second args))
      (.min model eq-arg (into-array IntVar args)))))

(defmethod ->choco* :min
  [{:keys [args]}]
  (let [args (map ->choco-int-var args)
        mins (map domain-min args)
        maxes (map domain-max args)
        final-min (apply min mins)
        final-max (apply min maxes)
        new-var (make-int-var final-min final-max)
        model (:csolver *solver*)]
    (constrain! (if (= (count args) 2)
                  (.min model new-var (first args) (second args))
                  (.min model new-var (into-array IntVar args))))
    new-var))

(defmethod ->choco* [:max :=]
  [{:keys [args eq-arg]}]
  (let [args (map ->choco-int-var args)
        eq-arg (->choco-int-var eq-arg)
        model (:csolver *solver*)]
    (if (= (count args) 2)
      (.max model eq-arg (first args) (second args))
      (.max model eq-arg (into-array IntVar args)))))

(defmethod ->choco* :max
  [{:keys [args]}]
  (let [args (map ->choco-int-var args)
        mins (map domain-min args)
        maxes (map domain-max args)
        final-min (apply max mins)
        final-max (apply max maxes)
        new-var (make-int-var final-min final-max)
        model (:csolver *solver*)]
    (constrain! (if (= (count args) 2)
                  (.max model new-var (first args) (second args))
                  (.max model new-var (into-array IntVar args))))
    new-var))


(defmethod ->choco* [:mod :=]
  [{X :arg1 Y :arg2 Z :eq-arg}]
  (let [model (:csolver *solver*)]
    (.mod model
          (->choco-int-var X)
          (->choco-int-var Y)
          (->choco-int-var Z))))

(defmethod ->choco* :mod
  [{X :arg1 Y :arg2}]
  (let [X (->choco-int-var X)
        Y (->choco-int-var Y)
        Ymax (domain-max Y)
        Z (make-int-var 0 (max (dec Ymax) 0))
        model (:csolver *solver*)]
    (constrain! (.mod model X Y Z))
    Z))

(defmethod ->choco* [:abs :=]
  [{X :arg Y :eq-arg}]
  (let [model (:csolver *solver*)]
    (.absolute model
               (->choco-int-var Y)
               (->choco-int-var X))))

(defmethod ->choco* :abs
  [{X :arg}]
  (let [model (:csolver *solver*)]
    (.abs model (->choco-int-var X))))

(defmethod ->choco* [:scalar :=]
  [{:keys [variables coefficients eq-arg]}]
  (let [model (:csolver *solver*)]
    (.scalar model
             (into-array IntVar (map ->choco-int-var variables))
             (int-array coefficients)
             "=" ;; options... {"=", "!=", ">","<",">=","<="}
             (->choco-int-var eq-arg))))

(defmethod ->choco* :scalar
  [{:keys [variables coefficients]}]
  (let [variables (map ->choco-int-var variables)
        minmaxes (for [[v c] (map vector variables coefficients)
                       :let [dmin (* (domain-min v) c)
                             dmax (* (domain-max v) c)]]
                   (if (< dmin dmax)
                     [dmin dmax]
                     [dmax dmin]))
        final-min (apply + (map first minmaxes))
        final-max (apply + (map second minmaxes))
        new-var (make-int-var final-min final-max)
        model (:csolver *solver*)]
    (constrain! (.intScalarView model
                                (into-array IntVar variables)
                                (int-array coefficients)
                                new-var))
    new-var))

(defmethod ->choco* :arithm-eq
  [{:keys [eq arg1 arg2]}]
  {:pre [#{"=", "!=", ">","<",">=","<="} eq]}
  (let [
        X (->choco-int-var arg1)
        Y (->choco-int-var arg2)
        model (:csolver *solver*)]
    ;;TODO: there is a 5-arity .arithm
    (.arithm model
             X
             eq
             Y)))

(defmethod ->choco* :true
  [_]
  (let [model (:csolver *solver*)]
    (.trueConstraint model)))

(defmethod ->choco* :false
  [_]
  (let [model (:csolver *solver*)]
    (.falseConstraint model)))

(defmethod ->choco* :and
  [{constraints :constraints}]
  (let [constraints (map ->choco constraints)
        model (:csolver *solver*)]
    (.and model (into-array Constraint constraints))))

(defmethod ->choco* :or
  [{constraints :constraints}]
  (let [constraints (map ->choco constraints)
        model (:csolver *solver*)]
    (.or model (into-array Constraint constraints))))

(defmethod ->choco* :not
  [{C :arg}]
  (let [model (:csolver *solver*)]
    (.not model (->choco C))))

(defmethod ->choco* :if
  [{if-this :if then-this :then else-this :else}]
  (let [model (:csolver *solver*)]
    (if-not else-this
      (.ifThen  model
                (->choco if-this)
                (->choco then-this))
      (.ifThenElse model
                   (->choco if-this)
                   (->choco then-this)
                   (->choco else-this)))))

(defmethod ->choco* :reify
  [{C :arg}]
  (let [C (->choco C)
        V (make-bool-var)
        model (:csolver *solver*)]
    (.reification model V C)
    V))

(defmethod ->choco* :distinct
  [{vars :args}]
  (let [vars (map ->choco vars)
        model (:csolver *solver*)]
    (.alldifferent model (into-array IntVar vars) "DEFAULT")))

(defmethod ->choco* :circuit
  [{list-of-vars :vars offset :offset}]
  (let [list-of-vars (map ->choco-int-var list-of-vars)
        model (:csolver *solver*)]
    (.circuit model (into-array IntVar list-of-vars) offset)))

(defmethod ->choco* [:nth :=]
  [{:keys [vars index offset eq-arg]}]
  (let [vars (map ->choco-int-var vars)
        index (->choco-int-var index)
        value (->choco-int-var eq-arg)
        model (:csolver *solver*)]
    (.element model value (into-array IntVar vars) index offset)))

(defmethod ->choco* :nth
  [{:keys [vars index offset]}]
  (let [vars (map ->choco-int-var vars)
        index (->choco-int-var index)
        final-min (apply min (map domain-min vars))
        final-max (apply max (map domain-max vars))
        new-var (make-int-var final-min final-max)
        model (:csolver *solver*)]
    (constrain! (.element model new-var (into-array IntVar vars) index offset))
    new-var))

(defmethod ->choco* :regular
  [{:keys [list-of-vars automaton]}]
  (let [list-of-vars (map ->choco-int-var list-of-vars)
        model (:csolver *solver*)]
    (.regular model (into-array IntVar list-of-vars) automaton)))

(defmethod ->choco* :cardinality
  [{variables :variables values :values occurrences :occurrences closed :closed}]
  (let [values (int-array values)
        occurrences (into-array IntVar (map ->choco-int-var occurrences))
        variables (into-array IntVar (map ->choco-int-var variables))
        model (:csolver *solver*)]
    (.globalCardinality model variables values occurrences (boolean closed))))

(defmethod ->choco* :knapsack
  [{:keys [weights values occurrences total-weight total-value]}]
  (let [occurrences (map ->choco-int-var occurrences)
        total-weight (->choco-int-var total-weight)
        total-value (->choco-int-var total-value)
        model (:csolver *solver*)]
    (.knapsack model
               (into-array IntVar occurrences)
               ^IntVar total-weight
               ^IntVar total-value
               (int-array weights)
               (int-array values))))
