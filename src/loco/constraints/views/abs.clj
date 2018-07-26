(ns loco.constraints.views.abs
  (:require
   [clojure.walk :as walk]
   [clojure.core.match :refer [match]]
   [clojure.spec.alpha :as s]
   [clojure.set :as set]
   [loco.constraints.utils :refer :all :as utils]
   )
  )

(def ^:private view-name 'abs)

;; example: [[:view -y [abs [] :y] [:int 0 -4]]
(s/def ::compile-spec
  (s/tuple #{:view} string? (s/tuple #{view-name} ::utils/coerce-intvar? #{[]}) ::utils/int-domain))

(defn- compiler-fn [model vars-index statement]
  (let [constraint-name view-name
        var-subed-statement (->> statement (walk/prewalk-replace vars-index))]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           [:view var-name [_ dependency-var _mods] _domain]
           (.intAbsView model (coerce-int-var model dependency-var))

           ::s/invalid
           (report-spec-error view-name ::compile-spec var-subed-statement))))

(defn- view-fn [name statement]
  (match statement
         [view-name dep []] (with-meta [:view name statement]
                              (meta statement))))

(defn- name-fn [statement]
  (match statement
         [view-name dep []] (str "|" (str+ dep) "|")))

(defn- domain-fn [statement possible-domain]
  (let [{:keys [lb ub]} (domainize possible-domain)
        [lb ub] (sort [(Math/abs lb) (Math/abs ub)])]
    (-> statement
        (conj [:int lb ub])
        (vary-meta assoc :domain {:int true :lb lb :ub ub}))))

;;TODO: $abs constraint and $abs view overlap, as $abs is a partial.
(defloco $abs-view
  "Creates a view over var such that: |var|.

- if var is already instantiated, returns a fixed variable;
- if the lower bound of var is greater or equal to 0, returns var;
- if the upper bound of var is less or equal to 0, return a minus view;
- otherwise, returns an absolute view;"
  {:view true}
  [dependency]
  (if (int? dependency)
    (Math/abs dependency)
    (view view-name
          dependency
          []
          name-fn
          view-fn
          domain-fn
          compiler-fn)))
