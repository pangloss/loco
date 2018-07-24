(ns loco.constraints.utils
  (:refer-clojure :exclude [compile var? set?])
  (:require
   [loco.utils :refer [split]]
   [clojure.core.match :refer [match]]
   [loco.match :refer [match+]]
   [clojure.spec.alpha :as s]
   [clojure.walk :as walk])
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

#_(defn- str+ [thing]
  (if (or (keyword? thing) (string? thing) (symbol? thing))
    (name thing)
    (str thing)))

#_(defn- default-partial-name-fn [[partial-name [& partial-contents]]]
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

(defn- stringize [obj]
  (cond
    (string? obj) obj
    (ident? obj) (name obj)
    (vector? obj) (str (mapv stringize obj))
    :else (str obj) ;; mainly for numebrs
    ))

(defn- default-name-fn [partial]
  ;;(println 'default-name-fn partial)
  (match partial
         [partial-name body]
         (->> body
              (map stringize)
              (interpose (name partial-name))
              (apply str))))

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
  ([op-name body & {:keys [name-fn constraint-fn domain-fn]
                    :or {name-fn default-name-fn}}]
   {:pre [(ident? op-name) (vector? body)]}
   nil
   ^{
     :constraint-fn constraint-fn
     :domain-fn domain-fn
     :name-fn name-fn
     :partial-constraint true
     } [op-name body]
   ))

(defn view [view-name dependency modifiers name-fn view-fn domain-fn compile-fn]
  {:pre [(symbol? view-name)
         (vector? modifiers)
         (every? fn? [name-fn view-fn domain-fn compile-fn])]}
  ^{
    :compiler compile-fn
    :domain-fn domain-fn
    :from dependency
    :modifiers modifiers
    :name-fn name-fn
    :view true
    :view-fn view-fn
    } [view-name dependency modifiers]
  )

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
;;FIXME: should be (def const-or-intvar? ...)
(def int-or-intvar? #(or (int? %) (int-var? %)))
(def bool-var?      (p instance? BoolVar))
(def set-var?       (p instance? SetVar))
(def task-var?      (p instance? Task))
(def tuples-var?    (p instance? Tuples))
;;(def int-or-bool? #(or (bool-var? %) (int-var? %)))

(defn domainize
  "turn non-domain things into domains, eg: numbers. useful for partial
  constraints who have a domain-fn."
  [obj]
  (cond
    (int? obj) {:int true :lb obj :ub obj}
    (and (find obj :lb)
         (find obj :ub)) obj
    )
  )

(defn coerce-int-var [model [coerced-int-var-type val]]
  {:pre [(instance? org.chocosolver.solver.Model model)]}
  (case coerced-int-var-type
    :int-var val
    :int (.intVar model val) ;;create int-var constant
    )
  )

(s/def ::comparison-operator? comparison-operator?)
(s/def ::arithmetic-operator? arithmetic-operator?)
(s/def ::comparison-symbol? comparison-symbol?)
(s/def ::arithmetic-symbol? arithmetic-symbol?)
(s/def ::int-var? (p instance? IntVar))
(s/def ::coerce-intvar? (s/or :int-var ::int-var? :int int?))
(s/def ::int-or-intvar? (some-fn int? int-var?))
(s/def ::int-vars (s/coll-of ::int-var?))
(s/def ::bool-vars (s/coll-of bool-var?))

(s/def ::list-type
  (s/or
   :bools ::bool-vars
   :ints  ::int-vars))

(s/def ::int-domain (s/tuple #{:int} int? int?))

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

;;FIXME: defloco doesn't work well with fdef (spec), so i think this
;;should be turned into a function that just does the intern, no defn
;;stuff.
(defmacro defloco
  "used for defining global loco vars (typically $name)"
  [name & more]
  {:pre [(symbol? name)]}
  (let [cur-ns (ns-name *ns*)]
    `(do
       #_(defn ~name ~@more)
       (in-ns 'loco.constraints) ;;need to do this shit because
                                 ;;sometimes the ns loco.constraints
                                 ;;isn't made, and we refer to it in
                                 ;;the intern! :(
       (in-ns '~cur-ns)
       #_(when (= (name '$arithm) (name '~name))
         (println '$arithm)
         (println)
         (println (meta #'loco.constraints.arithm/$arithm)))
       (let [defn# (defn ~name ~@more)
             v# (intern 'loco.constraints '~name defn#)]
         (reset-meta! v# (meta #'~name))
         )

       #_(intern 'loco.constraints '~name ~name)
       #_(when (= (name '$arithm) (name '~name))
         (println)
         (println (meta #'loco.constraints/$arithm)))
       )
    )
  )
