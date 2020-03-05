(ns loco.constraints.views.offset
  (:require
   [clojure.walk :as walk]
   [meander.epsilon :as m :refer [match]]
   [clojure.spec.alpha :as s]
   [clojure.set :as set]
   [loco.constraints.utils :refer :all :as utils]
   )
  )

(def ^:private view-name 'offset-view)

;; example: [[:view -y [offset [4] :y] [:int 0 4]]
(s/def ::compile-spec
  (s/tuple #{:view} string? (s/tuple #{view-name} ::utils/coerce-intvar? (s/tuple int?)) ::utils/int-domain))

(let [constraint-name view-name]
  (compile-function
   (match *conformed
     [:view ?var-name [_ ?dependency-var [?modifier]] _domain]
     (.intOffsetView *model (coerce-int-var *model ?dependency-var) ?modifier))))

(defn- view-fn [name statement]
  (match statement
    [_view-name _dep _mods] (with-meta [:view name statement]
                              (meta statement))))

(defn- name-fn [statement]
  (match statement
         [?view-name (m/pred int? ?dep) [(m/pred neg? ?modifier)]] (str (str+ ?dep) ?modifier)
         [?view-name (m/pred int? ?dep) [?modifier]]               (str ?dep "+" ?modifier)
         [?view-name ?dep [(m/pred neg? ?modifier)]]               (str (str+ ?dep) ?modifier)
         [?view-name ?dep [?modifier]]                             (str (str+ ?dep) "+" ?modifier)))

(defn- domain-fn [& partial]
  (let [[statement possible-domain] partial
        [_ _ [_ _ [mod]]] statement
        {:keys [lb ub]} (domainize possible-domain)
        [lb ub] (sort [(+ lb mod) (+ ub mod)])]
    (-> statement
        (conj [:int lb ub])
        (vary-meta assoc :domain {:int true :lb lb :ub ub}))))

(defn $offset-view
  "creates a offset view.

Creates a view based on var, equal to var+cste."
  {:view true}
  ([dependency magnitude]
   {:pre [(int? magnitude)]}
   (view view-name
         dependency
         [magnitude]
         name-fn
         view-fn
         domain-fn
         compiler)))
