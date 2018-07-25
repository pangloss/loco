(ns loco.constraints.views.scale
  (:require
   [clojure.core.match :refer [match]]
   [clojure.set :as set]
   [clojure.spec.alpha :as s]
   [clojure.walk :as walk]
   [loco.constraints.utils :refer :all :as utils]
   )
  )

(def ^:private view-name 'scale)

;; example: [[:view -y [scale [4] :y] [:int 0 4]]
(s/def ::compile-spec
  (s/tuple #{:view} string? (s/tuple #{view-name} ::utils/coerce-intvar? (s/tuple int?)) ::utils/int-domain))

(defn- compiler-fn [model vars-index statement]
  (let [constraint-name view-name
        var-subed-statement (->> statement (walk/prewalk-replace vars-index))]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           [:view var-name [_ dependency-var [modifier]] _domain]
           (.intScaleView model (coerce-int-var model dependency-var) modifier)

           ::s/invalid
           (report-spec-error view-name ::compile-spec var-subed-statement))))

(defn- view-fn [name statement]
  (match statement
         [view-name dep mods] (with-meta [:view name statement]
                                (meta statement))))

(defn- name-fn [statement]
  (match statement
         [view-name (dep :guard int?) [-1]]       (str "-(" dep ")")
         [view-name (dep :guard int?) [modifier]] (str dep "*" modifier)
         [view-name dep [modifier]]               (str (name dep) "*" modifier)))

(defn- domain-fn [& partial]
  (let [[statement possible-domain] partial
        [_ _ [_ _ [mod]]] statement
        {:keys [lb ub]} (domainize possible-domain)
        [lb ub] (sort [(* lb mod) (* ub mod)])]
    (-> statement
        (conj [:int lb ub])
        (vary-meta assoc :domain {:int true :lb lb :ub ub}))))

(defloco $scale
  "creates a scale view.

   Creates a view over var equal to var*cste. Requires cste > -2


   - if cste < -1, throws an exception;
   - if cste = -1, returns a minus view;
   - if cste = 0, returns a fixed variable;
   - if cste = 1, returns var;
   - otherwise, returns a scale view;"
  {:view true
   :choco ["intScaleView(IntVar var, int cste)"]}
  ([dependency magnitude]
   {:pre [(int? magnitude) (<= -1 magnitude)]}
   (view view-name
         dependency
         [magnitude]
         name-fn
         view-fn
         domain-fn
         compiler-fn)))
