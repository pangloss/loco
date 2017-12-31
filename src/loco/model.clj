(ns loco.model
  (:use loco.constraints)
  (:require
   [clojure.core.match :refer [match]]
   [clojure.walk :as walk]
   [loco.constraints :only [$const]]))

(def p partial)
(def c comp)

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
(defn- constraint-to-keyword
  "this will not work with nested constraints. use with postwalk"
  [statement]
  (match [statement]
         [[:constraint :partial [:neg dep-name]]] (keyword (str "-" (name dep-name)))
         [[:constraint :partial [op [& deps]]]] (->> (interpose (name op) (map name deps))
                                                   (apply str)
                                                   keyword)))

(defn- unnest-partial-vars
  "expects form of [:constraint [... ]]"
  [[_  statement-args]]
  (let [unnest-acc (atom [])
        extract-constraints (fn [form]
                              (if (partial-constraint? form)
                                (let [var-name (constraint-to-keyword form)]
                                  (swap! unnest-acc conj ^{:from form} [:var var-name :proto])
                                  var-name)
                                form))
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
                 (match [form]
                        [(form :guard number?)]
                        (let [var-name (keyword (str form))]
                          (swap! acc conj [:var var-name :hidden [:const form]])
                          var-name)

                        :else form
                        ))))
        ]
    (conj @acc transformed-statement)))

(defn- constraint-from-proto-var [statement]
  (match [statement (meta statement)]
         [[:var _ :proto] {:from [:constraint :partial [:neg dep-name]]}]
         [^{:neg dep-name} [:var (neg-var-name dep-name) :proto]]

         [[:var var-name :proto] {:from [_:constraint _:partial [:- [arg1 & more]]]}]
         (let [
               negative-vars (->> more
                                  (map (fn [var-name]
                                         ^{:neg var-name} [:var (neg-var-name var-name) :proto])))
               negative-var-names (map second negative-vars)
               ]
           (-> []
               (into negative-vars)
               (into [statement])
               (into [[:constraint [:sum (into [var-name := arg1] negative-var-names)]]])))

         [[:var var-name :proto] {:from [_:constraint _:partial [:+ args]]}]
         (-> []
             (into [statement])
             (into [[:constraint [:sum (into [var-name := ] args)]]]))

         [[:var var-name :proto] {:from [_:constraint _:partial [:% [arg1 arg2]]]}]
         (-> []
             (into [statement])
             (into [[:constraint [:mod (into [var-name := arg1 :% arg2])]]]))

         [[:var var-name :proto] {:from [_:constraint _:partial [:/ [arg1 arg2]]]}]
         (-> []
             (into [statement])
             (into [($div arg1 arg2 var-name)]))


         ;; [[:var var-name :proto [:from [:constraint _:partial [:* x y]]]]]
          ;; [
          ;;  statement
          ;;  [:constraint [:times ]]
          ;;  ]
         :else [statement]))

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

(defn- var-get-dependancies [statement]
  (match [statement (meta statement)]
         [[:var _ _] {:neg dep}] [dep]
         [[:var _ :proto]  {:from [_:constraint _:partial [:neg dep]]}] [dep]
         [[:var _ :proto]  {:from [_:constraint _:partial [_ deps]]}] deps))

(defn- lb-ub-seq [domains]
  (->> domains
       (map #(match [%]
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
(defn- apply-dependant-domain [statement dep-domains]
  (match [statement (meta statement) dep-domains]
         [[:var _ _] {:neg _} [[:int lb ub]]]
         [:int (- ub) (- lb)]

         [[:var _ _] {:neg _} [[:const b]]]
         [:const (- b)]

         ;;problem here is that i'm assuming int as domain, if we have
         ;;domains that are only consts, then this will still work,
         ;;but possibly better if we can tell if we only have booleans
         ;;FIXME: need function to infer domain type
         [[:var _ :proto] {:from [_:constraint _:partial[:- _]]} domains]
         (into [:int] (->> domains lb-ub-seq (reduce subtract-domains)))

         [[:var _ :proto] {:from [_:constraint _:partial [:+ _]]} domains]
         (into [:int] (->> domains lb-ub-seq (reduce add-domains)))

         [[:var _ :proto] {:from [_:constraint _:partial [:* _]]} domains]
         (into [:int] (->> domains lb-ub-seq (reduce multiply-domains)))

         [[:var _ :proto] {:from [_:constraint _:partial [:% _]]} domains]
         (into [:int] (->> domains lb-ub-seq (reduce modulo-domains)))

         [[:var _ :proto] {:from [_:constraint _:partial [:/ _]]} domains]
         (into [:int] (->> domains lb-ub-seq (reduce divide-domains)))
         ))

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
                              (map (p get var-index))
                              (mapv get-domain))
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
   (->> problem
        to-ast
        domain-transform)
))
