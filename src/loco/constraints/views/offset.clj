(ns loco.constraints.views.offset
  (:require
   [clojure.walk :as walk]
   [clojure.core.match :refer [match]]
   [clojure.spec.alpha :as s]
   [clojure.set :as set]
   [loco.constraints.utils :refer :all :as utils]
   )
  )

(def ^:private view-name 'offset)

;; example: [[:view -y [offset [4] :y] [:int 0 4]]
(s/def ::compile-spec
  (s/tuple #{:view} string? (s/tuple #{view-name} ::utils/coerce-intvar? (s/tuple int?)) ::utils/int-domain))

(defn- compiler-fn [model vars-index statement]
  (let [constraint-name view-name
        var-subed-statement (->> statement (walk/prewalk-replace vars-index))]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           [:view var-name [_ dependency-var [modifier]] _domain]
           (.intOffsetView model (coerce-int-var model dependency-var) modifier)

           ::s/invalid
           (report-spec-error view-name ::compile-spec var-subed-statement))))

(defn- view-fn [name statement]
  (match statement
         [view-name dep mods] (with-meta [:view name statement]
                                (meta statement))))

(defn- name-fn [statement]
  (match statement
         [view-name (dep :guard int?) [modifier]] (str dep "+" modifier)
         [view-name dep [modifier]]               (str (name dep) "+" modifier)))

(defn- domain-fn [& partial]
  (let [[statement possible-domain] partial
        {:keys [lb ub]} (domainize possible-domain)
        [lb ub] (sort [(+ lb) (+ ub)])]
    (-> statement
        (conj [:int lb ub])
        (vary-meta assoc :domain {:int true :lb lb :ub ub}))))

(defloco $offset
  "creates a offset view."
  {:view true}
  ([dependency magnitude]
   {:pre [(int? magnitude)]}
   (view view-name
         dependency
         [magnitude]
         name-fn
         view-fn
         domain-fn
         compiler-fn)))
