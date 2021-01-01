(ns loco.constraints.views.affine
  (:require
   [meander.epsilon :as m :refer [match]]
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

(let [constraint-name view-name]
  (compile-function compiler constraint-name [*conformed *model]
   (match *conformed
     [:view ?var-name [_ ?dependency-var [?coeff ?offset]] _domain]
     (.intAffineView *model ?coeff (coerce-int-var *model ?dependency-var) ?offset))))

(defn- view-fn [name statement]
  (match statement
         [_view-name _dep _mods] (with-meta [:view name statement]
                                   (meta statement))))

;;OMG wtf am i doing?
(defn- name-fn [statement]
  (match statement
         [_view-name (m/pred int? ?dep) [-1 0]]           (str "-(" ?dep ")")
         [_view-name (m/pred int? ?dep) [-1 ?offset]]     (str "-(" ?dep ")+" ?offset)
         [_view-name (m/pred int? ?dep) [?coeff ?offset]] (str (+ (* ?coeff ?dep) ?offset))
         [_view-name ?dep [-1 ?offset]]                    (str "-(" (str+ ?dep) ")" "+" ?offset)
         [_view-name ?dep [1 ?offset]]                     (str (str+ ?dep) "+" ?offset)
         [_view-name ?dep [0 ?offset]]                     (str ?offset)
         [_view-name ?dep [?coeff ?offset]]                 (str ?coeff (str+ ?dep) "+" ?offset)))

(defn- domain-fn [& partial]
  (let [[statement possible-domain] partial
        [_ _ [_ _ [mod]]] statement
        {:keys [lb ub]} (domainize possible-domain)
        [lb ub] (sort [(* lb mod) (* ub mod)])]
    (-> statement
        (conj [:int lb ub])
        (vary-meta assoc :domain {:int true :lb lb :ub ub}))))

(defn $affine
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
         compiler)))
