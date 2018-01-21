(ns loco.compiler
  (:refer-clojure :exclude [compile])
  (:use loco.constraints
        loco.utils)
  (:require
   [loco.model :as model]
   [loco.match :refer [match+]]
   [clojure.core.match :refer [match]])
  (:import org.chocosolver.solver.Model
           org.chocosolver.solver.variables.SetVar
           org.chocosolver.solver.variables.BoolVar
           org.chocosolver.solver.variables.IntVar
           org.chocosolver.solver.variables.Task
           org.chocosolver.solver.constraints.Constraint
           org.chocosolver.solver.constraints.nary.circuit.CircuitConf))

(defn- lookup-var [vars-index name]
  (if-let [var (get vars-index name)]
    var
    (if (number? name)
      name
      (throw (Exception. (str "Could not find variable: " name))))))

(defn- lookup-var-unchecked [vars-index name]
  (if-let [var (get vars-index name)]
    var
    (when (number? name)
      name)))

;;this is a bit annoying, maybe ok to move it into the
;;compile-var-statement, but currently only used by Task
(defn- anon-int-var
  ([model domain]
   {:pre [(or (and (sequential? domain) (every? integer? domain))
              (integer? domain))]}
   (if (integer? domain)
     (.intVar model domain)
     (.intVar model (int-array domain))))
  ([model lb ub]
   {:pre [(integer? lb) (integer? ub)]}
   (.intVar model (min lb ub) (max lb ub)))
  ([model lb ub bounded?]
   {:pre [(integer? lb) (integer? ub) (boolean? bounded?)]}
   (.intVar model (min lb ub) (max lb ub) bounded?)))

(defn compile-var-statement [[vars-index vars model] statement]
  (let [lookup-var (partial lookup-var vars-index)
        var (match+
             [statement (meta statement)]

             [[:var var-name _ _] {:neg dep-name}]
             (.intMinusView model (lookup-var dep-name))

             [[:var var-name _ [:bool _ _]] _]
             (.boolVar model (name var-name))

             [[:var var-name _ [:int lb ub]] _] :guard [[lb ub] integer?]
             (.intVar model (name var-name) lb ub)

             [[:var var-name _ [:int lb ub :bounded]] _] :guard [[lb ub] integer?]
             (.intVar model (name var-name) lb ub true)

             [[:var var-name _ [:int enumeration]] _] :guard [enumeration vector?]
             (.intVar model (name var-name) (int-array enumeration))

             [[:var var-name _ [:const value]] _] :guard [value integer?]
             (.intVar model (name var-name) value)

             [[:var var-name _ [:set constants]] _] :guard [constants set?]
             (.setVar model (name var-name) (into-array Integer/TYPE constants))

             [[:var var-name _ [:set lb ub]] _] :guard [[lb ub] [set? (p every? integer?)]]
             (.setVar model (name var-name)
                      (into-array Integer/TYPE lb)
                      (into-array Integer/TYPE ub))

             [[:var var-name _ [:task start duration end]] _]
             (let [task
                   (.taskVar
                    model
                    (if (keyword? start)
                      (lookup-var start)
                      (apply anon-int-var model start))
                    (if (keyword? duration)
                      (lookup-var duration)
                      (apply anon-int-var model duration))
                    (if (keyword? end)
                      (lookup-var end)
                      (apply anon-int-var model end)))]
               (.ensureBoundConsistency task)
               task)
             )]
    [(-> vars-index
         (with-meta {:ast-statement statement})
         (assoc (second statement) var))
     (conj vars var)
     model]))

;;TODO: refactor these boolean optimizations, they are all the same
(defn- sum-constraint
  "this handles the IntVar and BoolVar arguments method dispatching for Model.sum"
  [model sum-vars op eq-var]
  (if-let [homogeneous? (->> sum-vars (map class) (apply =))]
    (.sum model (into-array sum-vars) (name op) eq-var)
    (let [casted-to-intvars (map #(cast IntVar %) sum-vars)]
      (.sum model
            (into-array IntVar casted-to-intvars)
            (name op)
            eq-var))))

(defn- min-constraint
  "this handles the IntVar and BoolVar arguments method dispatching for Model.min"
  [model eq-var min-vars]
  (if-let [homogeneous? (->> min-vars (map class) (apply =))]
    (.min model eq-var (into-array min-vars))
    (let [casted-to-intvars (map #(cast IntVar %) min-vars)]
      (.min model eq-var (into-array IntVar casted-to-intvars)))))

(defn- max-constraint
  "this handles the IntVar and BoolVar arguments method dispatching for Model.max"
  [model eq-var max-vars]
  (if-let [homogeneous? (->> max-vars (map class) (apply =))]
    (.max model eq-var (into-array max-vars))
    (let [casted-to-intvars (map #(cast IntVar %) max-vars)]
      (.max model eq-var (into-array IntVar casted-to-intvars)))))

(defn compile-constraint-statement [vars-index model statement]
  (let [lookup-var (partial lookup-var vars-index)
        lookup-var-unchecked (partial lookup-var-unchecked vars-index)
        realize-nested-constraints (fn [constraints]
                                     (->> constraints
                                          (map (p compile-constraint-statement vars-index model))
                                          (into-array Constraint)
                                          ))
        realize-nested-constraint (p compile-constraint-statement vars-index model)
        int-var? (p instance? IntVar)
        set-var? (p instance? SetVar)
        bool-var? (p instance? BoolVar)
        lookup-set-var? (c set-var? lookup-var-unchecked)
        lookup-int-var? (c int-var? lookup-var-unchecked)
        lookup-bool-var? (c bool-var? lookup-var-unchecked)
        all-lookup-int-vars? (p every? lookup-int-var?)
        all-lookup-set-vars? (p every? lookup-set-var?)
        all-lookup-bool-vars? (p every? lookup-bool-var?)
        all-int-vars? (p every? int-var?)
        all-set-vars? (p every? set-var?)
        all-bool-vars? (p every? bool-var?)
        ]
    (->
     statement
     (match [:constraint constraint] constraint)
     (match+
      [:sum [(set-var :guard lookup-set-var?) := eq-var]]
      (.sum model (lookup-var set-var) (lookup-var eq-var))

      [:sum [eq-var := (set-var :guard lookup-set-var?)]]
      (.sum model (lookup-var set-var) (lookup-var eq-var))

      [:sum [eq-var op (sum-vars :guard vector?)]]
      (sum-constraint model (map lookup-var sum-vars) (name op) (lookup-var eq-var))

      [:arithm [comp-var comp-op var1 op var2]]
      (.arithm model
               (lookup-var comp-var)
               (name comp-op)
               (lookup-var var1)
               (name op)
               (lookup-var var2))

      [:arithm [comp-var comp-op var]]
      (.arithm model
               (lookup-var comp-var)
               (name comp-op)
               (lookup-var var))

      [:times [comp-var := var1 :* var2]]
      (.times model
               (lookup-var var1)
               (lookup-var var2)
               (lookup-var comp-var))

      [:mod [comp-var := var1 :% var2]]
      (.mod model
            (lookup-var var1)
            (lookup-var var2)
            (lookup-var comp-var))

      [:abs [abs-var := var]]
      (.absolute model (lookup-var abs-var) (lookup-var var))

      [:div [result := numerator :/ denominator ]]
      (.div model (lookup-var numerator) (lookup-var denominator) (lookup-var result))

      [:all-equal var-names]
      (match (mapv lookup-var var-names)
             (vars :guard (p every? int-var?))
             (.allEqual model (into-array IntVar vars))

             (vars :guard (p every? set-var?))
             (.allEqual model (into-array SetVar vars)))

      [:not-all-equal vars]
      (.notAllEqual model (->> vars (map lookup-var) (into-array IntVar)))

      [:min [min
             [:of weights]
             [:indices set-indices]
             [:offset offset]
             [:not-empty? not-empty?]]]
      (.min model
            (lookup-var set-indices)
            (into-array Integer/TYPE weights)
            offset
            (lookup-var min)
            not-empty?)

      [:min [min [:of set-var] [:not-empty? not-empty?]]]
      (.min model (lookup-var set-var) (lookup-var min) not-empty?)

      [:min [min [:of vars]]]
      (min-constraint model (lookup-var min) (map lookup-var vars))

      [:max [max
             [:of weights]
             [:indices set-indices]
             [:offset offset]
             [:not-empty? not-empty?]]]
      (.max model
            (lookup-var set-indices)
            (into-array Integer/TYPE weights)
            offset
            (lookup-var max)
            not-empty?)

      [:max [max [:of set-var] [:not-empty? not-empty?]]]
      (.max model (lookup-var set-var) (lookup-var max) not-empty?)

      [:max [max [:of vars]]]
      (max-constraint model (lookup-var max) (map lookup-var vars))

      [:scalar [result op vars coeffs]]
      (.scalar model
               (->> vars (map lookup-var) (into-array IntVar))
               (int-array coeffs)
               (name op)
               (lookup-var result))

      [:element [result [:in var-names] [:at index] [:offset offset]]]
      (match
       (mapv lookup-var var-names)
       (vars :guard (p every? integer?))
       (.element model
                 (lookup-var result)
                 (int-array vars)
                 (lookup-var index)
                 offset)

       (vars :guard (p every? int-var?))
       (.element model
                 (lookup-var result)
                 (into-array IntVar vars)
                 (lookup-var index)
                 offset)

       (vars :guard (p every? set-var?))
       (.element model
                 (lookup-var index)
                 (into-array SetVar vars)
                 offset
                 (lookup-var result)))

      [:distinct vars [:consistency consistency]]
      :guard [vars all-lookup-int-vars?, consistency #{:default :bc :ac}]
      (.allDifferent model
                     (->> vars (map lookup-var) (into-array IntVar))
                     ({:default "DEFAULT" :bc "BC" :ac "AC"} consistency))

      [:distinct var-names]
      (match (mapv lookup-var var-names)
             (vars :guard all-int-vars?)
             (.allDifferent model (into-array IntVar vars) "DEFAULT")

             (vars :guard all-set-vars?)
             ;;set-version doesn't have consistency argument
             (.allDifferent model (into-array SetVar vars)))

      [:distinct-except-0 vars]
      (.allDifferentExcept0 model (->> vars (map lookup-var) (into-array IntVar)))

      [:circuit [vars & more]] :guard [vars all-lookup-int-vars?]
      (match+ (vec more)
              [[:offset offset]]
              :guard [offset integer?]
              (.circuit model (->> vars (map lookup-var) (into-array IntVar)) offset)

              [[:offset offset] [:conf conf]]
              :guard [offset integer?, conf #{:all :first :light :rd}]
              (.circuit model
                        (->> vars (map lookup-var) (into-array IntVar))
                        offset
                        ({:all CircuitConf/ALL
                           :first CircuitConf/FIRST
                           :light CircuitConf/LIGHT
                           :rd CircuitConf/RD
                          } conf)))

      [:cardinality [vars [values occurrences] [:closed closed?]]]
      (.globalCardinality model
                          (->> vars (map lookup-var) (into-array IntVar))
                          (int-array values)
                          (->> occurrences (map lookup-var) (into-array IntVar))
                          closed?)

      [:knapsack [[:weight weights] [:energy energies]
                  [:occurrences occurrences] [:weight-sum weight-sum] [:energy-sum energy-sum]]]
      (.knapsack model
                 (->> occurrences (map lookup-var) (into-array IntVar))
                 (lookup-var weight-sum)
                 (lookup-var energy-sum)
                 (int-array weights)
                 (int-array energies))

      [:regular [vars [:automation automation]]]
      (.regular model
                (->> vars (map lookup-var) (into-array IntVar))
                automation)

      [:square [result dep]]
      (.square model (lookup-var result) (lookup-var dep))

      [:member [var [:lower-bound lb] [:upper-bound ub]]]
      (.member model (lookup-var var) lb ub)

      [:member [var :of (set-var :guard lookup-set-var?)]]
      (.member model (lookup-var var) (lookup-var set-var))

      [:member [member :of sets]] :guard [member lookup-set-var?
                                          sets [sequential? (p every? lookup-set-var?)]]
      (.member model
               (into-array SetVar (map lookup-var sets))
               (lookup-var member))

      [:member [var :of (table :guard (p every? integer?))]]
      (.member model (lookup-var var) (int-array table))

      [:not-member [var [:lower-bound lb] [:upper-bound ub]]]
      (.notMember model (lookup-var var) lb ub)

      [:not-member [var :of (set-var :guard lookup-set-var?)]]
      (.notMember model (lookup-var var) (lookup-var set-var))

      [:not-member [var :of (table :guard (p every? integer?))]]
      (.notMember model (lookup-var var) (int-array table))

      [:n-values [vars n-values]]
      (.nValues model (->> vars (map lookup-var) (into-array IntVar)) (lookup-var n-values))

      [:sort [vars sorted-vars]]
      (.sort model
             (->> vars (map lookup-var) (into-array IntVar))
             (->> sorted-vars (map lookup-var) (into-array IntVar)))

      [:count [vars [:value value] [:limit limit]]]
      (.count model
              (lookup-var value)
              (->> vars (map lookup-var) (into-array IntVar))
              (lookup-var limit))

      [:among [vars [:nb-var nb-var] [:values values]]]
      (.among model
              (lookup-var nb-var)
              (->> vars (map lookup-var) (into-array IntVar))
              (int-array values))

      [:at-least-n-values [vars [:n-values n-values] [:ac ac]]]
      (.atLeastNValues model
                       (->> vars (map lookup-var) (into-array IntVar))
                       (lookup-var n-values)
                       ac)

      [:at-most-n-values [vars [:n-values n-values] [:strong strong]]]
      (.atMostNValues model
                      (->> vars (map lookup-var) (into-array IntVar))
                      (lookup-var n-values)
                      strong)

      [:bin-packing
       [:item-bin item-bin]
       [:item-size item-size]
       [:bin-load bin-load]
       [:offset offset]]
      (.binPacking model
                   (->> item-bin (map lookup-var) (into-array IntVar))
                   (->> item-size int-array)
                   (->> bin-load (map lookup-var) (into-array IntVar))
                   offset)

      [:bit-channeling [:bool-vars bits] [:int-var int-var]]
      (.bitsIntChanneling model
                          (->> bits (map lookup-var) (into-array BoolVar))
                          (lookup-var int-var))

      [:diff-n
       [:xs xs]
       [:ys ys]
       [:widths widths]
       [:heights heights]
       [:add-cumulative-reasoning add-cumulative-reasoning?]]
      :guard [[xs ys widths heights] all-lookup-int-vars?
              add-cumulative-reasoning? boolean?]
      (.diffN model
              (->> xs (map lookup-var) (into-array IntVar))
              (->> ys (map lookup-var) (into-array IntVar))
              (->> widths (map lookup-var) (into-array IntVar))
              (->> heights (map lookup-var) (into-array IntVar))
              add-cumulative-reasoning?)

      [:bools-int-channeling [:bool-vars bools] [:int-var int-var] [:offset offset]]
      :guard [bools all-lookup-bool-vars?
              int-var lookup-int-var?
              offset integer?]
      (.boolsIntChanneling model
                           (->> bools (map lookup-var) (into-array BoolVar))
                           (lookup-var int-var)
                           offset)

      ;;TODO: do the int domain validation in the model/compile step
      ;;the distance of the LB and UB of int-var needs to be equal to (count e-vars)
      [:clauses-int-channeling [:int-var int-var] [:e-vars e-vars] [:l-vars l-vars]]
      :guard [[e-vars l-vars] all-lookup-bool-vars?, int-var lookup-int-var?]
      (.clausesIntChanneling model
                             (lookup-var int-var)
                             (->> e-vars (map lookup-var) (into-array BoolVar))
                             (->> l-vars (map lookup-var) (into-array BoolVar)))

      [:sub-circuit [vars
                     [:sub-circuit-length sub-circuit-length]
                     [:offset offset]]]
      :guard [offset integer?, sub-circuit-length lookup-int-var?, vars all-lookup-int-vars?]
      (.subCircuit model
                   (->> vars (map lookup-var) (into-array IntVar))
                   offset
                   (lookup-var sub-circuit-length))

      [:int-value-precede-chain [xs vs]]
      :guard [xs all-lookup-int-vars?, vs (p every? integer?)]
      (.intValuePrecedeChain model
                             (->> xs (map lookup-var) (into-array IntVar))
                             (int-array vs))

      [:int-value-precede-chain [xs s t]]
      :guard [[s t] integer?, xs all-lookup-int-vars?]
      (.intValuePrecedeChain model (->> xs (map lookup-var) (into-array IntVar)) s t)

      [:lex-chain-less vars] :guard [vars all-lookup-int-vars?]
      (.lexChainLess model (->> vars (map lookup-var) (into-array IntVar)))

      [:lex-chain-less-equal vars] :guard [vars all-lookup-int-vars?]
      `(.lexChainLessEq ~model ~@(->> vars (map lookup-var) (into-array IntVar)))

      [:lex-less [vars :lex-of lex-less-or-equal-vars]]
      :guard [[vars lex-less-or-equal] all-lookup-int-vars?]
      (.lexLess model
                (->> vars (map lookup-var) (into-array IntVar))
                (->> lex-less-or-equal-vars (map lookup-var) (into-array IntVar)))

      [:lex-less-equal [vars :lex-of lex-less-or-equal-vars]]
      :guard [[vars lex-less-or-equal] all-lookup-int-vars?]
      (.lexLessEq model
                  (->> vars (map lookup-var) (into-array IntVar))
                  (->> lex-less-or-equal-vars (map lookup-var) (into-array IntVar)))

      [:path [vars [:start start] [:end end] [:offset offset]]]
      :guard [vars all-lookup-int-vars?, [start end] lookup-int-var?, offset integer?]
      (.path model
             (->> vars (map lookup-var) (into-array IntVar))
             (lookup-var start)
             (lookup-var end)
             offset)

      [:sub-path [vars [:start start] [:end end] [:offset offset] [:size size]]]
      :guard [vars all-lookup-int-vars?, [start end size] lookup-int-var?, offset integer?]
      (.subPath model
                (->> vars (map lookup-var) (into-array IntVar))
                (lookup-var start)
                (lookup-var end)
                offset
                (lookup-var size))

      [:inverse-channeling [vars1 [:offset offset1]] [vars2 [:offset offset2]]]
      :guard [[vars1 vars2] all-lookup-int-vars?, [offset1 offset2] integer?]
      (.inverseChanneling model
                          (->> vars1 (map lookup-var) (into-array IntVar))
                          (->> vars2 (map lookup-var) (into-array IntVar))
                          offset1 offset2)

      [:tree [succs [:nb-trees nb-trees] [:offset offset]]]
      :guard [succs all-lookup-int-vars?, offset integer?, nb-trees lookup-int-var?]
      (.tree model (->> succs (map lookup-var) (into-array IntVar))
             (lookup-var nb-trees)
             offset)

      ;; -------------------- LOGIC --------------------
      ;; handle boolean lists
      [:and (bools :guard (p every? (c (p instance? BoolVar) lookup-var-unchecked)))]
      (.and model (->> bools (map lookup-var) (into-array BoolVar)))

      [:and (constraints :guard (p every? model/constraint?))]
      (.and model (realize-nested-constraints constraints))

      ;; handle boolean lists
      [:or (bools :guard (p every? (c (p instance? BoolVar) lookup-var-unchecked)))]
      (.or model (->> bools (map lookup-var) (into-array BoolVar)))

      [:or (constraints :guard (p every? model/constraint?))]
      (.or model (realize-nested-constraints constraints))

      [:when [(bool :guard (c (p instance? BoolVar) lookup-var-unchecked)) then-constraint]]
      (.ifThen model
               (lookup-var bool)
               (realize-nested-constraint then-constraint))

      [:when [if-constraint then-constraint]]
      (.ifThen model
               (realize-nested-constraint if-constraint)
               (realize-nested-constraint then-constraint))

      [:if-else [(bool :guard (c (p instance? BoolVar) lookup-var-unchecked))
                 then-constraint else-constraint]]
      (.ifThenElse model
                   (lookup-var bool)
                   (realize-nested-constraint then-constraint)
                   (realize-nested-constraint else-constraint))

      [:if-else [if-constraint then-constraint else-constraint]]
      (.ifThenElse model
                   (realize-nested-constraint if-constraint)
                   (realize-nested-constraint then-constraint)
                   (realize-nested-constraint else-constraint))

      [:iff [if-constraint then-constraint]]
      (.ifOnlyIf model
                 (realize-nested-constraint if-constraint)
                 (realize-nested-constraint then-constraint))

      [:not constraint]
      (.not model (realize-nested-constraint constraint))

      :true
      (.trueConstraint model)

      :false
      (.falseConstraint model)

      ;;-------------------- sets only --------------------

      [:intersection [intersection-set [:of sets] [:bound-consistent bounds-consistent?]]]
      (.intersection model
                     (into-array SetVar (map lookup-var sets))
                     (lookup-var intersection-set)
                     bounds-consistent?)

      [:union [union-set [:of (sets :guard (p every? lookup-set-var?))]]]
      (.union model
              (into-array SetVar (map lookup-var sets))
              (lookup-var union-set))

      [:union [union-set [:of (ints :guard (p every? lookup-int-var?))]]]
      (.union model
              (into-array IntVar (map lookup-var ints))
              (lookup-var union-set))

      [:nb-empty [num-empty [:of (sets :guard (p every? lookup-set-var?))]]]
      (.nbEmpty model
                (into-array SetVar (map lookup-var sets))
                (lookup-var num-empty))

      [:not-empty (set-var :guard lookup-set-var?)]
      (.notEmpty model (lookup-var set-var))

      [:off-set [(set1 :guard lookup-set-var?) (set2 :guard lookup-set-var?)
                 [:offset (offset :guard integer?)]]]
      (.offSet model (lookup-var set1) (lookup-var set2) offset)

      [:partition [(sets :guard [sequential? (p every? lookup-set-var?)])
                   [:universe (universe :guard lookup-set-var?)]]]
      (.partition model (into-array SetVar (map lookup-var sets)) (lookup-var universe))

      [:subset-eq (sets :guard (p every? lookup-set-var?))]
      (.subsetEq model (into-array SetVar (map lookup-var sets)))

      [:sum-elements [(result-var :guard lookup-int-var?)
                      [:indices (indices-set :guard lookup-set-var?)]
                      [:weights (weights :guard [sequential? (p every? integer?)])]
                      [:offset (offset :guard integer?)]]]
      (.sumElements model
                    (lookup-var indices-set)
                    (int-array weights)
                    offset
                    (lookup-var result-var))

      [:symetric [(sets :guard [sequential? (p every? lookup-set-var?)])
                  [:offset (offset :guard integer?)]]]
      (.symmetric model (into-array SetVar (map lookup-var sets)) offset)

      [:all-disjoint (sets :guard [sequential? (p every? lookup-set-var?)])]
      (.allDisjoint model (into-array SetVar (map lookup-var sets)))

      [:disjoint [(set1 :guard lookup-set-var?) (set2 :guard lookup-set-var?)]]
      (.disjoint model (lookup-var set1) (lookup-var set2))

      [:set-bools-channeling [(set-var :guard lookup-set-var?)
                              [:channel (bools :guard [sequential? (p every? lookup-bool-var?)])]
                              [:offset (offset :guard integer?)]]]
      (.setBoolsChanneling model
                           (into-array BoolVar (map lookup-var bools))
                           (lookup-var set-var)
                           offset)

      [:sets-ints-channeling [[:ints ints :offset offset-ints]
                              [:sets sets :offset offset-set]]]
      :guard [[ints sets] [sequential? (p every? lookup-set-var?)]
              [offset-set offset-inverse-set] integer?]
      (.setsIntsChanneling model
                           (into-array SetVar (map lookup-var sets))
                           (into-array IntVar (map lookup-var ints))
                           offset-set
                           offset-ints)

      [:inverse [[:inverse-sets inverse-sets :offset offset-inverse-set]
                 [:sets sets :offset offset-set]]]
      :guard [[inverse-sets sets] [sequential? (p every? lookup-set-var?)]
              [offset-set offset-inverse-set] integer?]
      (.setsIntsChanneling model
                           (into-array SetVar (map lookup-var sets))
                           (into-array SetVar (map lookup-var inverse-sets))
                           offset-set
                           offset-inverse-set)
      ))))



(defn compile-reify-statement [vars-index model statement]
  (match statement
         [:reify (var-name :guard (c (p instance? BoolVar)
                                     (p lookup-var-unchecked vars-index))) constraint]
         (.reification model
                       (lookup-var vars-index var-name)
                       (compile-constraint-statement vars-index model constraint))))

(defn compile-reifies [model vars-index ast]
  (->>
   ast
   (map (partial compile-reify-statement vars-index model))
   doall))

(defn compile-vars [model ast]
  (->>
   ast
   (reduce compile-var-statement [{} [] model])))

(defn compile-constraints [model vars-index ast]
  (->>
   ast
   (map (partial compile-constraint-statement vars-index model))
   doall))

(defn compile
  ([ast] (compile (Model.) ast))
  ([model ast]
   (let [
         uncompiled-vars        (->> ast (filter model/var?))
         uncompiled-constraints (->> ast (filter model/constraint?))
         uncompiled-reifies     (->> ast (filter model/reify?))
         [vars-index vars _]    (compile-vars model uncompiled-vars)
         public-var-names       (->> uncompiled-vars (filter model/public-var?) (map second))
         public-vars-index      (select-keys vars-index public-var-names)
         _reifies               (compile-reifies model vars-index uncompiled-reifies)
         constraints (->>
                      (compile-constraints model vars-index uncompiled-constraints)
                      ;;the conditional constraints return void, and are posted automatically
                      ;;the (when %) prevents NULL.post()
                      (map (juxt identity #(when % (.post %))))
                      (map first)
                      doall)]
     {
      :ast ast
      :var-name-mapping (:var-name-mapping (meta ast))
      :constraints constraints
      :model model
      :vars vars
      :vars-map (map vector uncompiled-vars vars)
      :public-vars-index public-vars-index
      :vars-index vars-index
      })))
