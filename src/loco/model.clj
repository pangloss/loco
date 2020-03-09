(ns loco.model
  (:refer-clojure :exclude [compile var? set?])
  (:require
   [meander.epsilon :as m :refer [match]]
   [clojure.pprint :refer [pprint]]
   [clojure.walk :as walk]
   [loco.constraints.vars :refer [$proto]]
   [loco.utils :refer [split]]
   [clojure.set :as set]
   [clojure.pprint :refer [pprint]]
   )
  )

(def ^:private c comp)
(def ^:private p partial)

(def ^:private get-domain (c :domain meta))
(def ^:private has-domain? (c some? :domain meta))
(def ^:private get-var-name (c second))

(def ^:private reify? (c some? :reify meta))
(defn- var? [statement]                 (->> statement meta :var))
(defn- set? [statement]                 (->> statement meta :set))
(defn- task? [statement]                (->> statement meta :task))
(defn- tuples? [statement]              (->> statement meta :tuples))
(defn- constraint? [statement]          (->> statement meta :constraint))
(defn- partial-constraint? [statement]  (->> statement meta :partial-constraint))
(defn- view? [statement]                (->> statement meta :view))
(defn- proto? [statement]               (->> statement meta :proto))

(defn- var-name [var] (second var))

(defn- var-name-domain-map [problem]
  (let [get-name second]
    (->> problem
         (filter var?)
         (filter has-domain?)
         (map (juxt get-name get-domain))
         (into {}))))

(defn- unwrap [wrapped]
  (if (and ((some-fn list? vector? set?) wrapped) (= 1 (count wrapped)))
    (first wrapped)
    wrapped))

(defn- search-nil? [obj]
  (let [acc (atom false)]
    (->> obj
         (walk/postwalk (fn [form]
                          (when (nil? form) (reset! acc true))
                          form)))
    @acc))

(defn- view-transform [statement acc]
  (let [{:keys [name-fn view-fn]} (meta statement)
        var-name (name-fn statement)
        view (view-fn var-name statement)]
    (swap! acc into [view])
    var-name))

(defn- unfold-partials [problem]
  (->> problem
       (mapcat
        (fn [constraint]
          (if-not (constraint? constraint)
            [constraint]
            (let [acc (atom [])
                  constraints-without-partials
                  (->> constraint
                       (walk/postwalk
                        (fn [statement]
                          (cond
                            (and (constraint? statement) (search-nil? statement)) nil
                            (view? statement) (view-transform statement acc)

                            (partial-constraint? statement)
                            (let [{:keys [name-fn constraint-fn]} (meta statement)
                                  var-name (name-fn statement)
                                  var ($proto var-name statement)
                                  constraint (constraint-fn var-name statement)
                                  children (unfold-partials constraint)
                                  constraint (if (not= children constraint)
                                               children constraint)
                                  return (match constraint
                                           [nil] nil
                                           (m/pred constraint? ?constraint) (do
                                                                              (swap! acc into (concat [var] [?constraint]))
                                                                              var-name)
                                           [(m/pred int? ?num)] ?num
                                           [(m/pred view? ?view)] (view-transform ?view acc)
                                           (m/pred #(some constraint? %) ?constraints) (do
                                                                                         (swap! acc into (concat [var] ?constraints))
                                                                                         var-name)
                                           [?unwrap] ?unwrap)]
                              return)
                            :else statement))))
                  ]
              (into @acc
                    [constraints-without-partials])))))))

(defn- realize-domain [[acc var-index] statement]
  ;;(println 'realize-domain statement (meta statement))
  ;;statement + meta looks something like
  ;;[:var "2+:x" :proto] {:var true, :proto true, :from [+ [2 :x]]}
  ;; :from has it's own meta too
  ;; ^{
  ;;   :partial-constraint true
  ;;   :name-fn name-fn
  ;;   :constraint-fn constraint-fn
  ;;   :domain-fn domain-fn
  ;;   }
  (cond
    (view? statement) (let [{:keys [from domain-fn]} (meta statement)
                            partial-with-replaced-var-domains (walk/postwalk-replace var-index from)
                            statement-with-domain (domain-fn statement partial-with-replaced-var-domains)
                            {:keys [domain]} (meta statement-with-domain)
                            updated-var-index (assoc var-index (get-var-name statement) domain)]
                        [(conj acc statement-with-domain) updated-var-index])
    (proto? statement) (let [{:keys [from upgrade-fn]} (meta statement)
                             {:keys [domain-fn]} (meta from)
                             partial-with-replaced-var-domains (walk/postwalk-replace var-index from)
                             domain (domain-fn partial-with-replaced-var-domains)
                             statement-with-domain (upgrade-fn statement domain)
                             updated-var-index (assoc var-index (get-var-name statement) domain)]
                         [(conj acc statement-with-domain) updated-var-index])
    :else [(conj acc statement) var-index]))

(defn- all-partials-transformed? [ast]
  (if-let [remaining-partials
           (->> ast
                (filter identity)
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

(defn- generated-vars? [statement] (->> statement meta :generated-vars))

(defn- all-statements-valid? [problem]
  (->> problem
       (filter identity)
       (every? (comp (some-fn var? constraint? generated-vars?)))))

(defn- elevate-generated-vars [problem]
  (let [var-name-index-init (->> problem
                                 (filter var?)
                                 (map var-name)
                                 (into #{}))
        [with-gen-vars non-gen-statements] (split generated-vars? problem)]
    (->>
     with-gen-vars
     (reduce
      (fn [[problem var-name-index] statement-with-gen-vars]
        (let [;;TODO: recur here (elevate-generated-vars statement-with-gen-vars)
              [vars non-vars] (split var? statement-with-gen-vars)
              new-vars (remove (comp var-name-index second) vars)
              new-var-names (->> new-vars (map var-name) (into #{}))
              ]
          [(-> problem
               (into new-vars)
               (into non-vars))
           (set/union var-name-index new-var-names)]))
      [non-gen-statements var-name-index-init])
     first)))

(defn- compile-problem [problem]
  (let [[vars constraints] (->> problem
                                unfold-partials
                                (filter identity)
                                (split (some-fn var? view?)))
        vars (vec (distinct vars))
        var-index (var-name-domain-map vars)
        [model _] (reduce realize-domain [[] var-index] vars)
        ]
    (vec (distinct (concat model constraints)))))

(defn compile [problem]
  {:pre [(sequential? problem)
         (all-partials-transformed? problem)
         (all-statements-valid? problem)]}
  (let [compiled (->> problem elevate-generated-vars compile-problem)
        var-name-obj-to-str-mapping (->> compiled
                                         var-name-replacement-map)
        compiled-str-var-names (walk/prewalk-replace
                                var-name-obj-to-str-mapping
                                compiled)
        return (with-meta
                 compiled-str-var-names
                 {:model? true
                  :var-name-mapping
                  (clojure.set/map-invert var-name-obj-to-str-mapping)})
        ]
    return))
