(ns loco.constraints.utils
  (:refer-clojure :exclude [compile var? set?])
  (:require
   [loco.utils :refer [split]]
   [clojure.core.match :refer [match]]
   [loco.match :refer [match+]]
   [clojure.spec.alpha :as s]
   [clojure.walk :as walk]
   [loco.constraints :as con])
  (:import
   [org.chocosolver.solver.variables IntVar BoolVar SetVar Task]
   org.chocosolver.solver.constraints.extension.Tuples))

(def ^:private c comp)
(def ^:private p partial)

(defn ^:dynamic preserve-consts [val]
  (match [val (meta val)]
         [_ {:preserve-const true}] val
         [_ {:preserve-consts true}] val
         [(val :guard number?) _] (vary-meta [val] merge {:preserve-const true})
         [(val :guard sequential?) _] (vary-meta val merge {:preserve-consts true})
         :else val))

(defn- var? [statement]                 (->> statement meta :var))
(defn- set? [statement]                 (->> statement meta :set))
(defn- task? [statement]                (->> statement meta :task))
(defn- tuples? [statement]              (->> statement meta :tuples))
(defn- constraint? [statement]          (->> statement meta :constraint))
(defn- partial-constraint? [statement]  (->> statement meta :partial-constraint))
(defn- view? [statement]                (->> statement meta :view))

(defn constraint
  ([name input compiler]
   ^{:constraint true
     :compiler compiler}
   [name input]))

(defn- str+ [thing]
  (if (or (keyword? thing) (string? thing) (symbol? thing))
    (name thing)
    (str thing)))

(defn- default-partial-name-fn [[partial-name [& partial-contents]]]
  (->> partial-contents
       (map str+)
       (interpose "_")
       (apply str (str+ partial-name) "_")))

(defn- hasher
  "produce a readable and short name"
  [deps-name]
  {:pre [(string? deps-name) (pos? (.length deps-name))]}
  (let [max-name-length 30]
    (if (<= (.length deps-name) max-name-length)
      deps-name
      (hash deps-name))))

(def ^:private get-domain (c :domain meta))
(def ^:private has-domain? (c some? :domain meta))
(def ^:private get-var-name (c second))

(defn- var-name-domain-map [problem]
  (let [get-name second]
    (->> problem
         (filter var?)
         (filter has-domain?)
         (map (juxt get-name get-domain))
         (into {}))))

(defn- unfold-partials [problem]
  (->> problem
       (mapcat
        (fn [constraint]
          (if-not (constraint? constraint)
            [constraint]
            (let [acc (atom [])
                  constraints-without-partials
                  (->> constraint
                       (walk/postwalk (fn [statement]
                                        (if-not (partial-constraint? statement)
                                          statement
                                          (let [{:keys [name-fn constraint-fn]} (meta statement)
                                                var-name (name-fn statement)
                                                var (con/proto-var var-name statement)
                                                constraint (constraint-fn var-name statement)
                                                ]
                                            (swap! acc
                                                   into [var constraint])
                                            var-name)))))
                  ]
              (into @acc
                    [constraints-without-partials])))))))

(defn partial-constraint
  "A partial constraint is one that lacks 1 variable (the equivalence
  variable). These are a syntactic sugar that make it easier to see
  the equivalence variable in a constraint statement.

  example: ($+ 4 = 1 3) <=> ($= 4 ($+ 1 3))

  A partial constraint requires some addition functions to support
  naming, transformation into a constraint, and determining the domain
  of subsequent supporting variables

  name-fn will create a name (string/keyword) for the hidden variable
  created to represent the partial constraint. the name-fn takes the
  whole body of the partial constraint, with nested partial
  constraints being already run through their name-fns

  constraint-fn will be given the output of the name-fn as the first
  argument, and the partial constraint body as the second
  argument. which should be enough information to create the
  non-partial constraint from.

  domain-fn will be given the partial constraint body, with all of the
  names of the variables replaced with their declarations, which come
  with meta data regarding their domains, which should then be used to
  calculate the domain of the partial-constraint. the output should be
  a map that follows domain conventions, eg: {:int true :lb 0 :ub 4} ...
  "
  [op-name body name-fn constraint-fn domain-fn]
  {:pre [(symbol? op-name) (vector? body)]}
  (let [partial [op-name body]
        var-name (name-fn partial)]
    ^{
      :partial-constraint true
      :name-fn name-fn
      :constraint-fn constraint-fn
      :domain-fn domain-fn
      } [op-name body]))

(defn view [view-name body compile-fn domain-fn]
  ^{
    :view true
    :compiler compile-fn
    :domain-fn domain-fn
    } [view-name body]
  )

(defn- realize-domain [[acc var-index] statement]
  ;;statement + meta looks something like
  ;;[:var "2+:x" :proto] {:var true, :proto true, :from [+ [2 :x]]}

  ;; :from has it's own meta too
  ;; ^{
  ;;   :partial-constraint true
  ;;   :name-fn name-fn
  ;;   :constraint-fn constraint-fn
  ;;   :domain-fn domain-fn
  ;;   }

  ;; possible that view and var can not work with the same code
  (if (and (or (view? statement) (var? statement))
           (not (set? statement))
           (not (task? statement))
           (not (tuples? statement))
           (not (has-domain? statement)))
    (let [partial (->> statement meta :from)
          domain-fn (->> partial meta :domain-fn)
          partial-with-replaced-var-domains (walk/postwalk-replace var-index partial)
          domain (domain-fn partial-with-replaced-var-domains)
          statement-with-domain (vary-meta statement assoc :domain domain)
          updated-var-index (assoc var-index (get-var-name statement) domain)
          ]
      [(conj acc statement-with-domain) updated-var-index])
    [(conj acc statement) var-index]))

(defn compile-problem [problem]
  (let [[vars constraints] (->> problem
                                unfold-partials
                                (sort-by var?)
                                (split var?))
        var-index (var-name-domain-map vars)
        _ (println 'var-index var-index) ;; should be {:7 [:var :7 :public [:const 7]]}
        [model _] (reduce realize-domain [[] var-index] vars)
        ]
    (vec (concat model constraints))
    #_[(map (juxt identity meta) problem)
       ;;     var-index
       ]
    ))

(def comparison-operator? #{'= '> '< '!=  '>= '<=
                            := :> :< :!= :not=  :>= :<=
                            =  >  <  not= >=  <=})

(def comparison-symbol? (->> comparison-operator?
                             (filter symbol?)
                             (into #{})))

(def arithmetic-operator? #{'+ '* '/ '-
                            :+ :* :/ :-
                            +  *  /  -})

(def arithmetic-symbol? (->> arithmetic-operator?
                             (filter symbol?)
                             (into #{})))

(def op-map
  {
   + '+
   - '-
   * '*
   / '/
   = '=
   not= '!=
   > '>
   < '<
   >= '>=
   <= '<=
   :+ '+
   :- '-
   :* '*
   :/ '/
   := '=
   :not= '!=
   :!= '!=
   :> '>
   :< '<
   :>= '>=
   :<= '<=
   })

(defn to-operator [op]
  (get op-map op
       ;;TODO: remove this after converting all constraints to spec
       ((clojure.set/union arithmetic-operator? comparison-operator?) op)))

(def int-var?       (p instance? IntVar))
(def int-or-intvar? #(or (int? %) (int-var? %)))
(def bool-var?      (p instance? BoolVar))
(def set-var?       (p instance? SetVar))
(def task?          (p instance? Task))
(def tuples?        (p instance? Tuples))
;;(def int-or-bool? #(or (bool-var? %) (int-var? %)))


(s/def ::int-vars (s/coll-of int-var?))
(s/def ::bool-vars (s/coll-of bool-var?))
;;(s/def ::mixed-ints-bools (s/coll-of int-or-bool?))

(s/def ::list-type
  (s/or
   :bools ::bool-vars
   :ints  ::int-vars))

(defn- convert-vars-to-strings
  "turn var objects into strings for easier reading/debugging"
  [obj]
  (->> obj
       (walk/prewalk
        #(if (every? false?
                     ((juxt int-var? bool-var? set-var?) %))
           %
           (str %)))))

(defn report-spec-error [constraint-name, spec-name, statement]
  (clojure.pprint/pprint (s/explain-data spec-name statement))
  (throw (ex-info
          (str "There is an error in the input to constraint [" constraint-name "]"
               "\n"
               (->> statement (s/explain-str spec-name))
               "\n"
               (s/describe spec-name)
               )
          (->> statement
               (s/explain-data spec-name)
               convert-vars-to-strings))))
