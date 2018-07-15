(ns loco.model
  (:refer-clojure :exclude [compile])
  ;;(:use loco.utils)
  (:require
   [loco.constraints.utils :as utils :refer []]
   [loco.utils :refer [partial-constraint? var? reify? constraint?]]
   ;; [loco.vars :as vars]
   ;; ;;[loco.constraints :refer [$abs $div $element $max $min $mod $neg $scalar $sum $times]]
   ;; [clojure.core.match :refer [match]]
    [clojure.walk :as walk]
    [clojure.pprint :refer [pprint]]
    )
  )

(defn- all-partials-transformed? [ast]
  (if-let [remaining-partials
           (->> ast
                (filter partial-constraint?)
                (map (fn [partial]
                       (str "Error: partial constraint "
                            (nth partial 2)
                            " isn't able to be transformed into a full constraint, "
                            "please wrap it in a partial consumer like $=")))
                seq)]
    (assert false remaining-partials)
    true))

(defn- var-name-replacement-map
  "This is used for replacing object-var-names with strings, so the
  names can be used with Choco."
  [ast]
  (let [vars (filter var? ast)
        reifies (filter reify? ast)
        var-names (->> (concat vars reifies) (map second))
        name->str-pairs (->> var-names
                             (map (fn [name]
                                    [name
                                     (if (keyword? name)
                                       name
                                       (str name))]))
                             (remove (partial apply =)))]
    (into {} name->str-pairs)))

;;FIXME: there is a problem with this accepting malformed models. the function should only accept a vector of vectors
(defn compile [problem]
  {:pre [(all-partials-transformed? problem)
         (vector? problem)
         (every? #(or (var? %) (constraint? %)) problem)]}
  (let [compiled (utils/compile-problem problem)
        var-name-obj-to-str-mapping (->> compiled
                                         var-name-replacement-map)
        ;; _ (println :var-name-mapping var-name-obj-to-str-mapping)
        ;; _ (pprint var-name-obj-to-str-mapping)
        ;; _ (pprint compiled)
        compiled-str-var-names (walk/prewalk-replace
                                var-name-obj-to-str-mapping
                                compiled)
        ]    
    ^{:var-name-mapping (clojure.set/map-invert var-name-obj-to-str-mapping)}
    compiled-str-var-names))

;; (defn keywordize [str]
;;   (if (clojure.string/starts-with? str ":")
;;     (keyword (.replace str ":" ""))
;;     (keyword str)))

;; (defn- constraint-from-proto-var [statement]
;;   ;;since preserve-const will turn a const into a wrapped const, and
;;   ;;we are calling functions with preserve-const, we need to insure
;;   ;;that it is a passthrough, or we will unwrap and rewrap consts
;;   ;;unintentionally
;;   (println 'con-from-proto ((juxt identity meta) statement))
;;   (binding [loco.constraints.utils/preserve-consts identity]
;;     (->
;;      [statement (meta statement)]
;;      (match
;;       [[:var var-name :proto] {:from [:constraint :partial constraint]}] [var-name constraint]
;;       [[:var var-name :proto] {:from [constraint]}]                      [var-name constraint]
;;       :else [statement])
;;      (match
;;       ;; [_ [:neg dep-name]]
;;       ;; [($neg (neg-var-name dep-name) dep-name)]

;;       :else [statement]))))



;; (defn within-domain [num [lb ub]]
;;   (and
;;    (<= lb num)
;;    (>= ub num)))

;; (defn cardinality-domain [var-name values occurences dep-domains]
;;   (let [cardinality-val (var-name (zipmap occurences values))
;;         ub (->>
;;             (lb-ub-seq dep-domains)
;;             (filter (p within-domain cardinality-val))
;;             count)]
;;     ;;vars of cardinality should always be IntVars
;;     [:int 0 ub]))

;; (defn- get-domain [statement]
;;   (match [statement]
;;          [[:var _ _ domain]] domain
;;          ;; [[:var _ _ [:int lb ub]]] [:int lb ub]
;;          ;; [[:var _ _ [:const val]]] [:const val]
;;          ;; [[:var _ _ [:int (domain :guard vector?)]]]
;;          :else nil))

;; (defn- has-domain? [statement]
;;   (if (get-domain statement) true false))

;; ;;TODO: this crazy matching could be replaced with spec/conform
;; (defn- apply-dependant-domain [statement dep-domains]
;;   {:pre [(every? has-domain? dep-domains)]}
;;   (println 'apply-dependant-domain statement (meta statement) dep-domains)
;;   (->
;;    [statement (meta statement)]
;;    (match

;;     [var-name [:constraint ['cardinality [vars [values occurences] _]]] dep-domains]
;;     (cardinality-domain var-name values occurences dep-domains)
;;     )))

;; ;;TODO: this way of getting deps for generated vars should probably be in the constraint-meta
;; ;;TODO: mixing of partials and deps is a bit confusing, as partials have a different syntax
;; (defn- var-get-dependancies [statement]
;;   (println 'var-get-dependancies statement (meta statement))
;;   (->
;;    [statement (meta statement)]

;;    (match
;;     [_                {:deps deps}]                        ['defined-deps deps]
;;     [[:var _ _]       {:neg dep}]                          [:neg dep]
;;     [[:var _ :proto]  {:from [:constraint :partial more]}] more
;;     [[:var _ :proto]  {:from constraint}]                  constraint
;;     [[:var _ _ deps]  nil]                                 deps
;;     )

;;    (match
;;     ['defined-deps deps] deps
;;     [:neg dep] [dep]
;;     [:scalar [deps _]] deps
;; ;;    [:$nth [deps ['at index] & _]] (into [index] deps)
;;     [_type deps] deps)))

;; (defn domain-transform [statements]
;;   (let [vars (filter var? statements)
;;         constraints&reifs (remove var? statements)
;;         var-index (index-by second vars)
;;         var-missing-domain? (comp nil? (p get-domain))
;;         ]
;;     ;; need to use reduce as domains get built based on previous ones
;;     (->> vars
;;          (reduce
;;           ;;as the domains of vars are assigned, the var-index needs to
;;           ;;be updated, because we want to avoid graph traversal and
;;           ;;assume we are in topologically sorted order
;;           (fn [[acc var-index] statement]
;;             (if (var-missing-domain? statement)
;;               (let [var-name (second statement)
;;                     dep-names (var-get-dependancies statement)
;;                     deps (->> dep-names
;;                               (map #(get var-index % %))
;;                               (mapv #(if-let [domain (get-domain %)]
;;                                        domain
;;                                        %)))

;;                     domain (apply-dependant-domain statement deps)
;;                     updated-statement (conj statement domain)
;;                     ]
;;                 [
;;                  (conj acc updated-statement)
;;                  (assoc var-index var-name updated-statement)
;;                  ]
;;                 )
;;               [(conj acc statement) var-index]))
;;           ;;init for reduce
;;           [[] var-index])
;;          ;;holy shit, have to jump through some hoops to get desired output...
;;          ;;can probably use (into) here
;;          first
;;          (vector constraints&reifs)
;;          reverse
;;          (apply concat)
;;          vec)))

;; (defn- unnest-all-constraints
;;   "the loco.constraints DSL allows for nested constraints, which aren't
;;   permitted in choco, this function will create intermediate variables
;;   and move deeply nested constraints to be top-level. should output
;;   proto-vars sorted topologically based on their dependancies"
;;   [statements]
;;   (->> statements
;;        (mapcat
;;         (fn [statement]
;;           (match statement
;;                  [:reify var-name constraint]
;;                  (let [[transformed-original & vars] (unnest-partial-vars constraint)]
;;                    (conj (vec vars)
;;                          ;;updates data structure as below:
;;                          ;;[:reify var-name [:constraint transformed-original]]
;;                          (assoc-in statement [2 1] transformed-original)))

;;                  [:constraint _statement-args]
;;                  (let [[transformed-original & vars] (unnest-partial-vars statement)]
;;                    (conj (vec vars)
;;                          ;;updates data structure as below:
;;                          ;;[:constraint transformed-original]
;;                          (assoc statement 1 transformed-original))))))))

;; (defn- const-transform [statement]
;;   (let [acc (atom [])
;;         transformed-statement
;;         (->>  statement
;;               (walk/prewalk
;;                (fn [form]
;;                  ;; the main reason for this gnarly code is to mark
;;                  ;; constants without creating infinite recursion
;;                  (match [form (meta form)]
;;                         [[(const :guard number?)] {:preserve-const true}]
;;                         const

;;                         [[(const :guard number?)] {:const true}]
;;                         (let [var-name (keyword (str const))]
;;                           (swap! acc conj ($const var-name const))
;;                           var-name)

;;                         [(form :guard sequential?) {:preserve-consts true}]
;;                         (->> form (map #(if (number? %)
;;                                           (with-meta [%] {:preserve-const true})
;;                                           %))
;;                              (into (empty form)))

;;                         [(form :guard vector?) _]
;;                         (->> form (map #(if (number? %)
;;                                           (with-meta [%] {:const true})
;;                                           %))
;;                              (into (empty form)))

;;                         :else form))))
;;         ]
;;     (conj @acc transformed-statement)))

;; (defn to-ast [problem]
;;   {:pre
;;    ;;very basic validation of objects in the ast list
;;    [(->> problem (every? #(->> % ((juxt var? reify? constraint?)) (not-any? nil?))))]}
;;   (let [
;;         vars (filter var? problem)
;;         [extracted-consts constraints-with-replaced-consts]
;;         (->> problem
;;              (filter #(or
;;                        (constraint? %)
;;                        (partial-constraint? %)
;;                        (reify? %)))
;;              (mapcat const-transform)
;;              ((juxt
;;                (p filter hidden-var?)
;;                (p remove hidden-var?))))
;;         partial-constraints-replacement-map (atom {})
;;         constraints-without-partials
;;         (->> constraints-with-replaced-consts
;;              (walk/postwalk (fn [statement]
;;                               (if (partial-constraint? statement)
;;                                 (do (swap! partial-constraints-replacement-map
;;                                            ;;want to build up replacement map for postwalk-replace
;;                                            assoc statement (constraint-to-keyword statement))
;;                                     (constraint-to-keyword statement))
;;                                 statement))))
;;         _ (println 'no-partials constraints-without-partials)
;;         _ (println 'partials @partial-constraints-replacement-map)
;;         ;; flattened-constraints (->> constraints-with-replaced-consts
;;         ;;                            (filter #(or
;;         ;;                                      (constraint? %)
;;         ;;                                      (partial-constraint? %)
;;         ;;                                      (reify? %)))
;;         ;;                            unnest-all-constraints)
;;         flattened-constraints constraint

;;         _ (println 'flattened-constraints flattened-constraints)
;;         constraints (filter constraint? flattened-constraints)
;;         reifies     (filter reify? flattened-constraints)
;;         proto-vars  (filter proto-var? flattened-constraints)
;;         ]

;;     (-> []
;;         (into vars)
;;         (into extracted-consts)
;;         (into proto-vars)
;;         (remove-dupes-by second) ;;only remove dupe vars
;;         (into reifies)
;;         (into constraints)
;;         (->> (mapcat constraint-from-proto-var)
;;              (into []))
;;         )))

;; ;;TODO: replace these validation functions with spec
;; (defn- all-var-names-are-unique? [ast]
;;   (if-let [duplicate-var-names (->> ast
;;                                     (filter var?)
;;                                     (map second)
;;                                     frequencies
;;                                     (remove (fn [[k v]] (= 1 v)))
;;                                     (map (fn [[var-label instance-count]]
;;                                            (str "Error: variable "
;;                                                 var-label
;;                                                 " has "
;;                                                 instance-count
;;                                                 " declarations.")))
;;                                     seq
;;                                  )]
;;     (assert false duplicate-var-names)
;;     true))


;; (defn- only-constraints-and-vars-and-reifies-present? [ast]
;;   (->> ast
;;        (every? (comp #{:constraint :var :reify} first))))


;; (defn compile
;;   "take in a representation of a model, a list of maps created using the
;;   constraints namespace. Transform into a model that can be consumed
;;   by model/realize, which creates a choco/Model object"
;;   [problem]
;;   {:pre [(all-partials-transformed? problem)]
;;    :post [
;;           (p only-constraints-and-vars-and-reifies-present?)
;;           (p all-var-names-are-unique?)
;;           ]}
;;   (let [unnest-generated-vars #(if (:generated-vars (meta %))
;;                                  %
;;                                  [%])
;;         flattened-problem (mapcat unnest-generated-vars problem)
;;         var-name-obj-to-str-mapping (create-variable-name-replacement-map flattened-problem)
;;         replaced-crazy-var-names (->> flattened-problem
;;                                       (walk/prewalk-replace var-name-obj-to-str-mapping))

;;         proto-ast (->>
;;                    replaced-crazy-var-names
;;                    (map (fn [statement]
;;                           (println 'map statement (meta statement))
;;                           (if-let [deps (->> statement meta :deps)]
;;                             (let [transformed-deps
;;                                   (->> deps
;;                                        (walk/prewalk-replace var-name-obj-to-str-mapping)
;;                                        )]
;;                               (println 'deps-transform transformed-deps)

;;                               (vary-meta statement assoc :deps transformed-deps)
;;                               ))
;;                           statement))
;;                    to-ast
;;                    )
;;         ;;ast (->> proto-ast domain-transform)
;;         ]
;;     ;;TODO: test malformed problems
;;     ;;(assert only-constraints-and-vars-and-reifies-present? flattened-problem)
;;     ;;(assert only-constraints-and-vars-and-reifies-present? proto-ast)
;;     (println 'var-name-mapping {:var-name-mapping (reverse-map var-name-obj-to-str-mapping)})
;;     #_(-> ast
;;           (with-meta {:var-name-mapping (reverse-map var-name-obj-to-str-mapping)}))
;;     ;;proto-ast
;;     ))
