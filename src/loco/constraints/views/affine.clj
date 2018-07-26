(ns loco.constraints.views.affine
  (:require
   [clojure.core.match :refer [match]]
   [clojure.set :as set]
   [clojure.spec.alpha :as s]
   [clojure.walk :as walk]
   [loco.constraints.utils :refer :all :as utils]
   )
  )

(def ^:private view-name 'affine)

;; example: [[:view -y [affine [4 8] :y] [:int 0 4]]
(s/def ::compile-spec
  (s/tuple #{:view} string? (s/tuple #{view-name} ::utils/coerce-intvar? (s/tuple int? int?)) ::utils/int-domain))

(defn- compiler-fn [model vars-index statement]
  (let [constraint-name view-name
        var-subed-statement (->> statement (walk/prewalk-replace vars-index))]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           [:view var-name [_ dependency-var [coeff offset]] _domain]
           (.intAffineView model coeff (coerce-int-var model dependency-var) offset)

           ::s/invalid
           (report-spec-error view-name ::compile-spec var-subed-statement))))

(defn- view-fn [name statement]
  (match statement
         [view-name dep mods] (with-meta [:view name statement]
                                (meta statement))))

;;OMG wtf am i doing?
(defn- name-fn [statement]
  (match statement
         [view-name (dep :guard int?) [-1 0]]         (str "-(" dep ")")
         [view-name (dep :guard int?) [-1 offset]]    (str "-(" dep ")+" offset)
         [view-name (dep :guard int?) [coeff offset]] (str (+ (* coeff dep) offset))
         [view-name dep [-1 offset]]                  (str "-(" (str+ dep) ")" "+" offset)
         [view-name dep [1 offset]]                   (str (str+ dep) "+" offset)
         [view-name dep [0 offset]]                   (str offset)
         [view-name dep [coeff offset]]               (str coeff (str+ dep) "+" offset)))

(defn- domain-fn [& partial]
  (let [[statement possible-domain] partial
        [_ _ [_ _ [mod]]] statement
        {:keys [lb ub]} (domainize possible-domain)
        [lb ub] (sort [(* lb mod) (* ub mod)])]
    (-> statement
        (conj [:int lb ub])
        (vary-meta assoc :domain {:int true :lb lb :ub ub}))))

(defloco $affine
  "Creates an affine view over x such that: a.x + b.

Parameters:
    a - a coefficient
    x - an integer variable.
    b - a constant"
  {:view true
   :choco ["intAffineView(int a, IntVar x, int b)"]}
  ([dependency coefficient offset]
   {:pre [(int? coefficient)
          (int? offset)]}
   (view view-name
         dependency
         [coefficient offset]
         name-fn
         view-fn
         domain-fn
         compiler-fn)))
