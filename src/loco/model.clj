(ns loco.model
  (:use loco.constraints
        loco.utils)
  (:require
   [clojure.core.match :refer [match]]
   [clojure.walk :as walk]
   [loco.constraints :only [$const]]))

(defn- neg-var-name [dep-name]
  (keyword (str "-" (name dep-name))))

(defn- var?
  [form]
  (match [form]
         [[:var & _]] true
         :else false))

(defn- public-var? [form]
  (match [form]
         [[:var _ :public & _]] true
         :else false))

(defn- hidden-var? [form]
  (match [form]
         [[:var _ :hidden & _]] true
         :else false))

(defn- proto-var? [form]
  (match [form]
         [[:var _ :proto & _]] true
         :else false))

(defn- constraint? [form]
  (match [form]
         [[:constraint & _]] true
         :else false))

(defn- partial-constraint? [form]
  (match [form]
         [[:constraint :partial & _]] true
         :else false))

;;TODO: attach the name making function to the constraint itself via meta from ($neg) function
(defn constraint-to-keyword
  "this will not work with nested constraints. use with postwalk"
  [statement]
  (->
   [statement]
   (match [[:constraint :partial more]] [more])
   (match
    [[:neg dep-name]] (str "-" (name dep-name))
    [[:abs [dep]]] (str "|" (name dep) "|")
    [[op [dep]]] (str (name op) "_" (name dep))

    [[(op :guard #(->> % name count (= 1))) [& deps]]]
    (->> (interpose (name op) (map name deps))
         (apply str))

    [[op (args :guard #(->> % flatten count (< 10)))]]
    (->> args hash (str (name op) "_"))

    [[:scalar [vars coeffs]]]
    (->> [(map name vars) (repeat "*") coeffs]
         (apply map vector)
         (interpose "+")
         flatten
         (apply str "scalar_"))

    [[:element stuff]]
    (->> stuff
         flatten
         (map str)
         (interpose "_")
         (apply str "element_"))

    [[op [& deps]]]
    (->> (interpose "_" (map name deps))
         (apply str (name op) "_")))

   keyword))

(defn- unnest-partial-vars
  "expects form of [:constraint [... ]]"
  [[_  statement-args]]
  (let [unnest-acc (atom [])
        extract-constraints
        (fn [form]
          (match [form]
                 [[:cardinality [deps [_ occurences] _]]]
                 (do
                   (swap! unnest-acc
                          into
                          (map #(with-meta [:var % :proto] {:from form}) occurences))
                   ;; we need to preserve the cardinality constraint
                   ;; in it's originality. we are not actually
                   ;; unnesting partials in this case
                   form)

                 [(form :guard partial-constraint?)]
                 (let [var-name (constraint-to-keyword form)]
                   (swap! unnest-acc
                          conj
                          ^{:from form} [:var var-name :proto])
                   var-name)
                 :else form))
        transformed-constraint (walk/postwalk extract-constraints statement-args)]
    (into [transformed-constraint] @unnest-acc)))

(defn- unnest-all-constraints
  "the loco.constraints DSL allows for nested constraints, which aren't
  permitted in choco, this function will create intermediate variables
  and move deeply nested constraints to be top-level. should output
  proto-vars sorted topologically based on deps"
  [statements]
  (let [vars (filter var? statements)
        constraints (filter constraint? statements)]

    (into
     (vec vars)
     (->> constraints
          (mapcat
           (fn [[type _statement-args :as statement]]
             (let [[transformed-original & vars] (unnest-partial-vars statement)]
               (conj (vec vars)
                     [type transformed-original]))))))))

(defn- const-transform [statement]
  (let [acc (atom [])
        transformed-statement
        (->>  statement
              (walk/prewalk
               (fn [form]
                 ;; the main reason for this gnarly code is to mark
                 ;; constants without creating infinite recursion
                 (match [form (meta form)]
                        [[(const :guard number?)] {:preserve-const true}]
                        const

                        [[(const :guard number?)] {:const true}]
                        (let [var-name (keyword (str const))]
                          (swap! acc conj [:var var-name :hidden [:const const]])
                          var-name)

                        [(form :guard vector?) {:preserve-consts true}]
                        (->> form (map #(if (number? %)
                                          (with-meta [%] {:preserve-const true})
                                          %))
                             (into (empty form)))

                        [(form :guard vector?) _]
                        (->> form (map #(if (number? %)
                                          (with-meta [%] {:const true})
                                          %))
                             (into (empty form)))

                        :else form
                        ))))
        ]
    (conj @acc transformed-statement)))

(defn- constraint-from-proto-var [statement]
  (->
   [statement (meta statement)]
   (match
    [[:var var-name :proto] {:from [:constraint :partial constraint]}] [var-name constraint]
    [[:var var-name :proto] {:from [constraint]}] [var-name constraint]
    :else [statement])
   (match
    [_ [:neg dep-name]]
    [($neg (neg-var-name dep-name) dep-name)]

    [var-name [:- [arg1 & more]]]
    (let [
          negative-vars (->> more
                             (map (fn [var-name]
                                    ^{:neg var-name} [:var (neg-var-name var-name) :proto])))
          negative-var-names (map second negative-vars)
          ]
      (-> []
          (into negative-vars)
          (into [statement])
          (into [($sum var-name := (into [arg1] negative-var-names))])))

    [var-name [:+ args]]
    (-> []
        (into [statement])
        (into [($sum var-name := args)]))

    [var-name [:% [arg1 arg2]]]
    (-> []
        (into [statement])
        (into [($mod arg1 arg2 var-name)]))

    [var-name [:* [arg1 arg2]]]
    (-> []
        (into [statement])
        (into [($times arg1 arg2 var-name)]))

    [var-name [:/ [arg1 arg2]]]
    (-> []
        (into [statement])
        (into [($div arg1 arg2 var-name)]))

    [var-name [:abs [arg]]]
    (-> []
        (into [statement])
        (into [($abs var-name arg)]))

    [var-name [:min args]]
    (-> []
        (into [statement])
        (into [($min var-name args)]))

    [var-name [:max args]]
    (-> []
        (into [statement])
        (into [($max var-name args)]))

    [var-name [:scalar [vars coeffs]]]
    (-> []
        (into [statement])
        (into [($scalar var-name := vars coeffs)]))

    [var-name [:element [vars :at index _ offset]]]
    (-> []
        (into [statement])
        (into [($element var-name vars index offset)]))

    ;;TODO: get rid of this and only use the one with offest
    [var-name [:element [vars :at index]]]
    (-> []
        (into [statement])
        (into [($element var-name vars index)]))

    :else [statement])))

(defn index-by [f coll]
  (->>
   coll
   (map (juxt f identity))
   (into {})))

(defn- get-domain [statement]
  (match [statement]
         [[_:var _ _ [:int lb ub]]] [:int lb ub]
         [[_:var _ _ [:const val]]] [:const val]
         :else nil))

;;TODO: add a deps meta tag to partial constraints!
(defn- var-get-dependancies [statement]
  (->
   [statement (meta statement)]

   (match
    [[:var _ _] {:neg dep}] [:neg dep]
    [[:var _ :proto]  {:from [:constraint :partial more]}] more
    [[:var _ :proto]  {:from constraint}] constraint
    )

   (match
    [:neg dep] [dep]
    [:scalar [deps _]] deps
    [:element [deps _ index & _]] (into [index] deps)
    [:cardinality [deps [values occurences] _]] deps
    [_type deps] deps)))

(defn- lb-ub-seq [domains]
  (->>
   domains
   (map #(match
          [%]
          [(const :guard integer?)] [const const]
          [[:const b & _]] [b b]
          [[:int lb ub]] [lb ub]))))

(defn- subtract-domains [[lb1 ub1] [lb2 ub2]]
  [(- lb1 ub2) (- ub1 lb2)])

(defn- add-domains [[lb1 ub1] [lb2 ub2]]
  [(+ lb1 lb2) (+ ub1 ub2)])

(defn- multiply-domains [[lb1 ub1] [lb2 ub2]]
  (let [possible-bounds [(* lb1 lb2) (* lb1 ub2) (* ub1 lb1) (* ub1 ub2)]]
    [(apply min possible-bounds)
     (apply max possible-bounds)]))

(defn modulo-domains [[lb1 ub1] [lb2 ub2]]
  [0 ub2])

(defn divide-domains [[lb1 ub1] [lb2 ub2]]
  (->>
   (match [lb1 lb2 ub1 ub2]
          [_ 0 _ 0] [lb1 ub1]
          [_ _ _ 0] [(/ lb1 lb2) ub1]
          [_ 0 _ _] [lb1 (/ ub1 ub2)]
          [_ _ _ _] [(/ lb1 lb2) (/ ub1 ub2)])
   (mapv (comp int #(if (neg? %)
                      (Math/floor %)
                      (Math/ceil %))))))

(defn abs-domain [_ [lb ub]]
  (sort [(Math/abs lb) (Math/abs ub)]))

(defn min-domains [[lb1 ub1] [lb2 ub2]]
  [(min lb1 lb2), (min ub1 ub2)])

(defn max-domains [[lb1 ub1] [lb2 ub2]]
  [(max lb1 lb2), (max ub1 ub2)])

(defn element-domains
  ([list idx-lb idx-ub]
   (element-domains list idx-lb idx-ub 0))

  ([list idx-lb idx-ub offset]
   (->>
    list
    lb-ub-seq
    (drop offset)
    (drop idx-lb)
    (take (inc idx-ub))
    ((juxt (comp first first (p sort-by first))
           (comp last last (p sort-by second)))))))

(defn within-domain [num [lb ub]]
  (and
   (<= lb num)
   (>= ub num)))

(defn cardinality-domain [var-name values occurences dep-domains]
  (let [cardinality-val (var-name (zipmap occurences values))
        ub (->>
            (lb-ub-seq dep-domains)
            (filter (p within-domain cardinality-val))
            count)]
    ;;vars of cardinality should always be IntVars
    [:int 0 ub]))

(defn- apply-dependant-domain [statement dep-domains]
  (->
   [statement (meta statement)]
   (match
    [[:var _ _]      {:neg _}]                               [:neg dep-domains]
    [[:var _ :proto] {:from [:constraint :partial partial]}] [partial dep-domains]
    [[:var var-name :proto] {:from constraint}]              [var-name constraint dep-domains])
   (match
    [:neg [[:int lb ub]]]
    [:int (- ub) (- lb)]

    [:neg [[:const b]]]
    [:const (- b)]

    ;;problem here is that i'm assuming int as domain, if we have
    ;;domains that are only consts, then this will still work,
    ;;but possibly better if we can tell if we only have booleans
    ;;FIXME: need function to infer domain type
    [[:- _] domains]
    (into [:int] (->> domains lb-ub-seq (reduce subtract-domains)))

    [[:+ _] domains]
    (into [:int] (->> domains lb-ub-seq (reduce add-domains)))

    [[:* _] domains]
    (into [:int] (->> domains lb-ub-seq (reduce multiply-domains)))

    [[:% _] domains]
    (into [:int] (->> domains lb-ub-seq (reduce modulo-domains)))

    [[:/ _] domains]
    (into [:int] (->> domains lb-ub-seq (reduce divide-domains)))

    [[:abs _] domains]
    (into [:int] (->> domains lb-ub-seq (reduce abs-domain [0 0])))

    [[:min _] domains]
    (into [:int] (->> domains lb-ub-seq (reduce min-domains)))

    [[:max _] domains]
    (into [:int] (->> domains lb-ub-seq (reduce max-domains)))

    [[:scalar [_ coeffs]] domains]
    (into [:int] (->> domains lb-ub-seq
                      (map #(multiply-domains %2 [%1 %1]) coeffs)
                      (reduce add-domains)))

    [[:element [_vars :at _ :offset offset]] [[:int index-lb index-ub] & vars]]
    (into [:int] (element-domains vars index-lb index-ub offset))

    [[:element [_vars :at _]] [[:int index-lb index-ub] & vars]]
    (into [:int] (element-domains vars index-lb index-ub))

    #_[[:cardinality [[:a :b :c :d :e]
                    [[1 2] [:ones :twos]]
                    [:closed false]]] [{1 :ones, 2 :twos} [:a :b :c :d :e]]]
    [(var-name :guard keyword?) [:cardinality [vars [values occurences] _]] dep-domains]
    (cardinality-domain var-name values occurences dep-domains)
    )))

(defn domain-transform [statements]
  (let [constraints (filter constraint? statements)
        vars (remove constraint? statements)
        var-index (index-by second vars)
        var-missing-domain? (comp nil? (p get-domain))
        ]
    ;; need to use reduce as domains get built based on previous ones
    (->> vars
         (reduce
          ;;as the domains of vars are assigned, the var-index needs to
          ;;be updated, because we want to avoid graph traversal and
          ;;assume we are in topologically sorted order
          (fn [[acc var-index] statement]
            (if (var-missing-domain? statement)
              (let [var-name (second statement)
                    dep-names (var-get-dependancies statement)
                    deps (->> dep-names
                              (map #(get var-index % %))
                              (mapv #(if-let [domain (get-domain %)]
                                       domain
                                       %)))
                    domain (apply-dependant-domain statement deps)
                    updated-statement (conj statement domain)
                    ]
                [
                 (conj acc updated-statement)
                 (assoc var-index var-name updated-statement)
                 ]
                )
              [(conj acc statement) var-index]))
          ;;init for reduce
          [[] var-index])
         ;;holy shit, have to jump through some hoops to get desired output...
         first
         (vector constraints)
         reverse
         (apply concat)
         vec)))

(defn to-ast [problem]
  (let [
        vars (filter var? problem)
        transformed-problem (->> problem
                                 (filter #(or (constraint? %) (partial-constraint? %)))
                                 (mapcat const-transform)
                                 unnest-all-constraints
                                 )
        hidden-vars (filter hidden-var? transformed-problem)
        constraints (filter constraint? transformed-problem)
        proto-vars (filter proto-var? transformed-problem)
        ]

    (-> []
        (into vars)
        (into hidden-vars)
        (into proto-vars)
        (into constraints)
        (->> (mapcat constraint-from-proto-var)
             (into [])))))

(defn compile
  "take in a representation of a model, a list of maps created using the
  constraints namespace. Transform into a model that can be consumed
  by model/realize, which creates a choco/Model object"
  ([problem]
   {:post [(every? (comp #{:constraint :var} first) %)]}
   (->> problem
        to-ast
        domain-transform)
))
