(ns loco.constraints.views.minus
  (:require
   [clojure.walk :as walk]
   [meander.epsilon :as m :refer [match]]
   [clojure.spec.alpha :as s]
   [clojure.set :as set]
   [loco.constraints.utils :refer :all :as utils]
   )
  )

(def ^:private view-name 'minus-view)

;; example: [[:view -y [minus [] :y] [:int 0 -4]]
(s/def ::compile-spec
  (s/tuple #{:view} string? (s/tuple #{view-name} ::utils/coerce-intvar? #{[]}) ::utils/int-domain))

(let [constraint-name view-name]
  (compile-function
   (match *conformed
     [:view ?var-name [_ ?dependency-var _mods] _domain]
     (.intMinusView *model (coerce-int-var *model ?dependency-var)))))

(defn- view-fn [name statement]
  (match statement
    [view-name dep []] (with-meta [:view name statement]
                         (meta statement))))

(defn- name-fn [statement]
  (match statement
    [?view-name (m/pred int? ?dep) []] (str "-" ?dep)
    [?view-name ?dep []] (str "-" (str+ ?dep))))

(defn- domain-fn [statement possible-domain]
  (let [{:keys [lb ub]} (domainize possible-domain)
        [lb ub] (sort [(- lb) (- ub)])]
    (-> statement
        (conj [:int lb ub])
        (vary-meta assoc :domain {:int true :lb lb :ub ub}))))

;;TODO: fix up docs
(defn $minus-view
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
         compiler)))
