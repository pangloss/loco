(ns loco.constraints.sum
  (:require
   [clojure.spec.alpha :as s]
   [loco.utils :refer [p c]]
   [loco.constraints.utils :refer
    [preserve-consts, comparison-operator? qualified-operator-map
     qualified-comparison-operator? constraint partial-constraint]]
   [clojure.core.match :refer [match]]
   [loco.match :refer [match+]]
   [clojure.walk :as walk])
  (:import
   org.chocosolver.solver.variables.IntVar
   org.chocosolver.solver.variables.BoolVar
   org.chocosolver.solver.variables.SetVar))

(def int-var?  (p instance? IntVar))
(def bool-var? (p instance? BoolVar))
(def set-var?  (p instance? SetVar))
(def int-or-bool? #(or (bool-var? %) (int-var? %)))

(s/def ::ints (s/coll-of int-var?))
(s/def ::bools (s/coll-of bool-var?))
(s/def ::mixed-ints-bools (s/coll-of int-or-bool?))

(s/def ::list-type
  (s/or
        :bools ::bools
        :ints  ::ints))

(s/def ::sum (s/cat :constraint #{:con/sum}
                    :args (s/spec
                           (s/or
                            :set   (s/cat :eq-var int-var?
                                          :op #{:op/=}
                                          :var set-var?)
                            :bools (s/cat :eq-var int-var?
                                          :op qualified-comparison-operator?
                                          :vars (s/spec ::bools))
                            :ints  (s/cat :eq-var int-var?
                                          :op qualified-comparison-operator?
                                          :vars (s/spec ::ints))))))

(defn convert-vars-to-strings
  "turn var objects into strings for easier reading/debugging"
  [obj]
  (->> obj
       (walk/prewalk
        #(if (every? false?
                     ((juxt int-var? bool-var? set-var?) %))
           %
           (str %)))))

(defn- sum-compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))]
    (match+ (->> var-subed-statement (s/conform ::sum))
            {:constraint :con/sum,:args [:ints {:eq-var eq-var :op op, :vars vars}]}
            (.sum model (into-array IntVar vars) (name op) eq-var)

            {:constraint :con/sum,:args [:bools {:eq-var eq-var :op op, :vars vars}]}
            (.sum model (into-array BoolVar vars) (name op) eq-var)

            {:constraint :con/sum,:args [:set {:eq-var eq-var :op :op/=, :var set-var}]}
            (.sum model set-var eq-var)

            ::s/invalid
            (do
              (throw (ex-info
                      (str "There is an error in the input to :sum"
                           "\n"
                           (->> var-subed-statement (s/explain-str ::sum)))
                      (->> var-subed-statement
                           (s/explain-data ::sum)
                           convert-vars-to-strings)))))))

(defn sum
  "Creates a sum constraint. Enforces that ∑i in |vars|varsi operator sum
  Creates a constraint summing elements of set sum{i | i in set} = sum"
  {:choco ["sum(BoolVar[] vars, String operator, IntVar sum)"
           "sum(IntVar[]  vars, String operator, IntVar sum)"
           "sum(SetVar set, IntVar sum)"]
   :partial true}
  ([vars]
   {:pre [(sequential? vars)]}
   ;; this is named differently because it creates nice var
   ;; names. gets converted into a :sum at compile step
   (partial-constraint [:+ vars]))

  ([summation set-var]
   (let [op (:= qualified-operator-map)]
     (-> (constraint [:con/sum [summation op set-var]])
         (with-meta {:compiler (var sum-compiler)}))))

  ([summation operator vars]
   {:pre [(sequential? vars)
          (comparison-operator? operator)]}
   (let [op (operator qualified-operator-map)]
     (-> (constraint [:con/sum [summation op vars]])
         (with-meta {:compiler (var sum-compiler)})))))
