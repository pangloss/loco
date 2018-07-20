(ns loco.constraints.views.minus
  (:require
   [clojure.walk :as walk]
   [clojure.core.match :refer [match]]
   [clojure.spec.alpha :as s]
   [clojure.set :as set]
   [loco.constraints.utils :refer :all :as utils]
   )
  )

(def ^:private view-name 'minus)

;; example: [[:view -y [minus :y]]
(s/def ::compile-spec
  (s/tuple #{:view} string? (s/tuple #{view-name} int-var? #{[]}) ::utils/int-domain))

;;TODO: this requires that there is a similar var compiler function as to constraints
(defn- compiler-fn [model vars-index statement]
  ;;(println 'compiler-fn view-name [statement vars-index model])
  (let [constraint-name view-name
        var-subed-statement (->> statement (walk/prewalk-replace vars-index))]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           [:view var-name [_ dependency-var _mods] _domain]
           (.intMinusView model dependency-var)

           ::s/invalid
           (report-spec-error view-name ::compile-spec var-subed-statement))))

#_(defn- domain-fn [partial]
  (match partial
         [view-name [dependency]]
         (-> (match dependency
                    ;;TODO: handle enumerated domains
                    {:int true :lb lb :ub ub}
                    (let [[neg-lb neg-ub] (sort [(- lb) (- ub)])] {:lb neg-lb :ub neg-ub}))
             (assoc :int true)
             (update :lb int)
             (update :ub int))))

;; if i set this up as a partial constraint, then i don't have to
;; worry about adding new logic to the code to support views (which
;; are a sorta mix of constraint/var). it may be possible that we can
;; completely treat this like a partial-constraint, as it just outputs
;; a proto

(defn- view-fn [name statement]
  ;;(println 'view-fn statement (meta statement))
  (match statement
         [view-name dep []] (with-meta [:view name statement]
                           (meta statement))))

(defn- name-fn [statement]
  (match statement
         [view-name dep []] (str "-" (name dep))))

;;this is from subtraction
#_(defn- domain-fn [partial]
  (match partial
         [partial-name body]
         (->
          (reduce
           (fn [{:keys [lb ub] :as acc} domain]
             (match domain
                    ;;TODO: handle enumerated domains
                    {:int true :lb cur-lb :ub cur-ub} {:lb (- (int lb) (int cur-ub))
                                                       :ub (- (int ub) (int cur-lb))}))
           {:lb 0 :ub 0}
           body)
          (assoc :int true))))

(defn- domain-fn [statement {:keys [lb ub]}]
  (let [[lb ub] (sort [(- lb) (- ub)])]
    (-> statement
        (conj [:int lb ub])
        (vary-meta assoc :domain {:int true :lb lb :ub ub}))))

;;TODO: fix up docs
(defloco $minus
  "takes a partial constraint and creates a negative constraint from
  it (neg (- :x :b)) also can be used to create a neg var
  via (neg :-i :i)

  :exmaple
  ($int :my-var 0 5)
  ($neg :my-var) -> [:var :-my-var [:int -5 0]]
  "
  {:view true}
  ([dependency]
   (view view-name
         dependency
         []
         name-fn
         view-fn
         domain-fn
         compiler-fn)))

;;TODO: add meta data
(defloco $neg [& more] (apply $minus more))
(reset-meta! (var $neg) (meta (var $minus)))
