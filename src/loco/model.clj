(ns loco.model
  (:refer-clojure :exclude [compile var?])
  (:use loco.utils)
  (:require
   [loco.vars :as vars]
   [loco.constraints :refer [$abs $div $element $max $min $mod $neg $scalar $sum $times]]
   [clojure.core.match :refer [match]]
   [clojure.walk :as walk]))

(defn keywordize [str]
  (if (clojure.string/starts-with? str ":")
    (keyword (.replace str ":" ""))
    (keyword str)))

(defn- neg-var-name [dep-name]
  (keyword (str "-" (name dep-name))))


;;TODO: attach the name making function to the constraint itself via
;;meta from ($neg) function

;;TODO: these would be better if they were part of the function that
;;created the partial constraint (like on it's meta data)


(defn nice-keyword-str? [str]
  (not (or
        (clojure.string/includes? str "[")
        (clojure.string/includes? str "]")
        (clojure.string/includes? str "{")
        (clojure.string/includes? str "}")
        (clojure.string/includes? str "(")
        (clojure.string/includes? str ")")
        (when (clojure.string/ends-with? str ":") true)
        )))

#_(= [(nice-keyword-str? "re")
       (nice-keyword-str? ":re")
       (nice-keyword-str? ":re:")
       (nice-keyword-str? ":re[f]")
      ]
     [true true false false])

(defn constraint-to-keyword
  "takes an un-nested partial-constraint and tries to make a nice name
  from it and it's arguments. this will not work with nested
  constraints. use with postwalk"
  [statement]
  (let [new-name (->
                  statement
                  (match [:constraint :partial more] more)
                  (match
                   [:neg dep-name] (str "-" (name dep-name))

                   [op [dep]] (str (name op) "_" (name dep))

                   [(op :guard #(->> % name count (= 1))) [& deps]]
                   (->> deps
                        (interpose (name op))
                        (apply str))

                   [op (args :guard #(->> % flatten count (< 10)))]
                   (->> args hash (str (name op) "_"))

                   [:scalar [vars coeffs]]
                   (->> [(map name vars) (repeat "*") coeffs]
                        (apply map vector)
                        (interpose "+")
                        flatten
                        (apply str "scalar_"))

                   ;;TODO: take this concept of making a name, and put it into $nth meta
                   ;;or the meta of the partial-constraint
                   ;;fetch the function from $nth and run it if exists


                   (->> (interpose "_" (map name deps))
                        (apply str (name op) "_")))

                  )]
    (if (nice-keyword-str? new-name)
      (keywordize new-name)
      new-name)))

(defn- constraint-from-proto-var [statement]
  ;;since preserve-const will turn a const into a wrapped const, and
  ;;we are calling functions with preserve-const, we need to insure
  ;;that it is a passthrough, or we will unwrap and rewrap consts
  ;;unintentionally
  (println 'con-from-proto ((juxt identity meta) statement))
  (binding [loco.constraints.utils/preserve-consts identity]
    (->
     [statement (meta statement)]
     (match
      [[:var var-name :proto] {:from [:constraint :partial constraint]}] [var-name constraint]
      [[:var var-name :proto] {:from [constraint]}]                      [var-name constraint]
      :else [statement])
     (match
      [_ [:neg dep-name]]
      [($neg (neg-var-name dep-name) dep-name)]



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

      [var-name [:$nth [vars [:at index] [:offset offset]]]]
      (-> []
          (into [statement])
          (into [($element var-name vars index offset)]))

      :else [statement]))))

(defn- lb-ub-seq [domains]
  (->>
   domains
   (map #(match
          [%]
          [(const :guard integer?)] ^:const [const const]
          [[:const b & _]] ^:const [b b]
          [[:int lb ub true]] ^:bounded ^:lb-ub [lb ub] ;;bounded int-var
          [[:int lb ub]] ^:lb-ub [lb ub]
          [[:bool 0 1]] ^:bool [0 1]
          [[:int (domain :guard vector?)]] ^{:enumerated domain} ((juxt first last) domain)))))




(defn min-domains [[lb1 ub1] [lb2 ub2]]
  [(min lb1 lb2), (min ub1 ub2)])

(defn max-domains [[lb1 ub1] [lb2 ub2]]
  [(max lb1 lb2), (max ub1 ub2)])

(defn element-domains
  [list idx-lb idx-ub offset]
  (->>
   list
   lb-ub-seq
   (drop offset)
   (drop idx-lb)
   (take (inc idx-ub))
   ((juxt (comp first first (p sort-by first))
          (comp last last (p sort-by second))))))

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

(defn- get-domain [statement]
  (match [statement]
         [[:var _ _ domain]] domain
         ;; [[:var _ _ [:int lb ub]]] [:int lb ub]
         ;; [[:var _ _ [:const val]]] [:const val]
         ;; [[:var _ _ [:int (domain :guard vector?)]]]
         :else nil))

(defn- has-domain? [statement]
  (if (get-domain statement) true false))

;;TODO: this crazy matching could be replaced with spec/conform
(defn- apply-dependant-domain [statement dep-domains]
  {:pre [(every? has-domain? dep-domains)]}
  (println 'apply-dependant-domain statement (meta statement) dep-domains)
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

    [[:min _] domains]
    (into [:int] (->> domains lb-ub-seq (reduce min-domains)))
    [[:max _] domains]
    (into [:int] (->> domains lb-ub-seq (reduce max-domains)))

    [[:scalar [_ coeffs]] domains]
    (into [:int] (->> domains lb-ub-seq
                      (map #(multiply-domains %2 [%1 %1]) coeffs)
                      (reduce add-domains)))

    [[:$nth [_ _ [:offset (offset :guard nat-int?)]]] [[:int index-lb index-ub] & vars]]
    (into [:int] (element-domains vars index-lb index-ub offset))

    [var-name [:constraint ['cardinality [vars [values occurences] _]]] dep-domains]
    (cardinality-domain var-name values occurences dep-domains)
    )))

;;TODO: this way of getting deps for generated vars should probably be in the constraint-meta
;;TODO: mixing of partials and deps is a bit confusing, as partials have a different syntax
(defn- var-get-dependancies [statement]
  (println 'var-get-dependancies statement (meta statement))
  (->
   [statement (meta statement)]

   (match
    [_                {:deps deps}]                        ['defined-deps deps]
    [[:var _ _]       {:neg dep}]                          [:neg dep]
    [[:var _ :proto]  {:from [:constraint :partial more]}] more
    [[:var _ :proto]  {:from constraint}]                  constraint
    [[:var _ _ deps]  nil]                                 deps
    )

   (match
    ['defined-deps deps] deps
    [:neg dep] [dep]
    [:scalar [deps _]] deps
;;    [:$nth [deps ['at index] & _]] (into [index] deps)
    [_type deps] deps)))

(defn domain-transform [statements]
  (let [vars (filter var? statements)
        constraints&reifs (remove var? statements)
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
         ;;can probably use (into) here
         first
         (vector constraints&reifs)
         reverse
         (apply concat)
         vec)))

(defn- unnest-all-constraints
  "the loco.constraints DSL allows for nested constraints, which aren't
  permitted in choco, this function will create intermediate variables
  and move deeply nested constraints to be top-level. should output
  proto-vars sorted topologically based on their dependancies"
  [statements]
  (->> statements
       (mapcat
        (fn [statement]
          (match statement
                 [:reify var-name constraint]
                 (let [[transformed-original & vars] (unnest-partial-vars constraint)]
                   (conj (vec vars)
                         ;;updates data structure as below:
                         ;;[:reify var-name [:constraint transformed-original]]
                         (assoc-in statement [2 1] transformed-original)))

                 [:constraint _statement-args]
                 (let [[transformed-original & vars] (unnest-partial-vars statement)]
                   (conj (vec vars)
                         ;;updates data structure as below:
                         ;;[:constraint transformed-original]
                         (assoc statement 1 transformed-original))))))))

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
                          (swap! acc conj ($const var-name const))
                          var-name)

                        [(form :guard sequential?) {:preserve-consts true}]
                        (->> form (map #(if (number? %)
                                          (with-meta [%] {:preserve-const true})
                                          %))
                             (into (empty form)))

                        [(form :guard vector?) _]
                        (->> form (map #(if (number? %)
                                          (with-meta [%] {:const true})
                                          %))
                             (into (empty form)))

                        :else form))))
        ]
    (conj @acc transformed-statement)))

(defn to-ast [problem]
  {:pre
   ;;very basic validation of objects in the ast list
   [(->> problem (every? #(->> % ((juxt var? reify? constraint?)) (not-any? nil?))))]}
  (let [
        vars (filter var? problem)
        [extracted-consts constraints-with-replaced-consts]
        (->> problem
             (filter #(or
                       (constraint? %)
                       (partial-constraint? %)
                       (reify? %)))
             (mapcat const-transform)
             ((juxt
               (p filter hidden-var?)
               (p remove hidden-var?))))
        partial-constraints-replacement-map (atom {})
        constraints-without-partials
        (->> constraints-with-replaced-consts
             (walk/postwalk (fn [statement]
                              (if (partial-constraint? statement)
                                (do (swap! partial-constraints-replacement-map
                                           ;;want to build up replacement map for postwalk-replace
                                           assoc statement (constraint-to-keyword statement))
                                    (constraint-to-keyword statement))
                                statement))))
        _ (println 'no-partials constraints-without-partials)
        _ (println 'partials @partial-constraints-replacement-map)
        ;; flattened-constraints (->> constraints-with-replaced-consts
        ;;                            (filter #(or
        ;;                                      (constraint? %)
        ;;                                      (partial-constraint? %)
        ;;                                      (reify? %)))
        ;;                            unnest-all-constraints)
        flattened-constraints constraint

        _ (println 'flattened-constraints flattened-constraints)
        constraints (filter constraint? flattened-constraints)
        reifies     (filter reify? flattened-constraints)
        proto-vars  (filter proto-var? flattened-constraints)
        ]

    (-> []
        (into vars)
        (into extracted-consts)
        (into proto-vars)
        (remove-dupes-by second) ;;only remove dupe vars
        (into reifies)
        (into constraints)
        (->> (mapcat constraint-from-proto-var)
             (into []))
        )))

(defn- all-var-names-are-unique? [ast]
  (if-let [duplicate-var-names (->> ast
                                    (filter var?)
                                    (map second)
                                    frequencies
                                    (remove (fn [[k v]] (= 1 v)))
                                    (map (fn [[var-label instance-count]]
                                           (str "Error: variable "
                                                var-label
                                                " has "
                                                instance-count
                                                " declarations.")))
                                    seq
                                 )]
    (assert false duplicate-var-names)
    true))

(defn- all-partials-transformed? [ast]
  (if-let [remaining-partials
           (->> ast
                (filter partial-constraint?)
                (map (fn [partial]
                       (str "Error: partial constraint "
                            (nth partial 2)
                            " isn't able to be transformed into a full constraint, please wrap it in a partial consumer like $=")))
                seq)]
    (assert false remaining-partials)
    true))

(defn- only-constraints-and-vars-and-reifies-present? [ast]
  (->> ast
       (every? (comp #{:constraint :var :reify} first))))

(defn- create-variable-name-replacement-map
  "This is used for replacing object-var-names with strings"
  [ast]
  (->>
   ast
   ((juxt (p filter var?) (p filter reify?)))
   (apply concat)
   (map second)
   (map (juxt identity #(if (keyword? %)
                          %
                          (str %))))
   (remove (p apply =))
   (into {})))

(defn compile
  "take in a representation of a model, a list of maps created using the
  constraints namespace. Transform into a model that can be consumed
  by model/realize, which creates a choco/Model object"
  [problem]
  {:pre [(all-partials-transformed? problem)]
   :post [
          (p only-constraints-and-vars-and-reifies-present?)
          (p all-var-names-are-unique?)
          ]}
  (let [unnest-generated-vars #(if (:generated-vars (meta %))
                                 %
                                 [%])
        flattened-problem (mapcat unnest-generated-vars problem)
        var-name-obj-to-str-mapping (create-variable-name-replacement-map flattened-problem)
        replaced-crazy-var-names (->> flattened-problem
                                      (walk/prewalk-replace var-name-obj-to-str-mapping))

        proto-ast (->>
                   replaced-crazy-var-names
                   (map (fn [statement]
                          (println 'map statement (meta statement))
                          (if-let [deps (->> statement meta :deps)]
                            (let [transformed-deps
                                  (->> deps
                                       (walk/prewalk-replace var-name-obj-to-str-mapping)
                                       )]
                              (println 'deps-transform transformed-deps)

                              (vary-meta statement assoc :deps transformed-deps)
                              ))
                          statement))
                   to-ast
                   )
        ;;ast (->> proto-ast domain-transform)
        ]
    ;;TODO: test malformed problems
    ;;(assert only-constraints-and-vars-and-reifies-present? flattened-problem)
    ;;(assert only-constraints-and-vars-and-reifies-present? proto-ast)
    (println 'var-name-mapping {:var-name-mapping (reverse-map var-name-obj-to-str-mapping)})
    #_(-> ast
          (with-meta {:var-name-mapping (reverse-map var-name-obj-to-str-mapping)}))
    ;;proto-ast
    ))
