;; FIXME: WIP
(ns loco.constraints.at-least-n-values
    (:require
     [clojure.core.match :refer [match]]
     [clojure.spec.alpha :as s]
     [clojure.walk :as walk]
     [loco.constraints.utils :refer :all :as utils]
     [loco.utils :refer [p]]
     )
    (:import
     [org.chocosolver.solver.variables IntVar]))
;; (in-ns 'loco.constraints)
;; (ns loco.constraints.at-least-n-values
;;   (:use loco.constraints.utils)
;;     (:require
;;      [clojure.spec.alpha :as s]
;;      [loco.constraints.utils :as utils]
;;      [loco.match :refer [match+]]
;;      [clojure.core.match :refer [match]]
;;      [clojure.walk :as walk])
;;     (:import
;;      [org.chocosolver.solver.variables IntVar]))

;; (def ^:private constraint-name 'at-least-n-values)

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/cat
                       :ints     ::utils/coll-coerce-intvar?
                       :n-values (s/tuple #{'n-values} ::utils/coerce-intvar?)
                       :ac       (s/tuple #{'ac} boolean?)))))

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))
        coerce-int-var (p utils/coerce-int-var model)]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:args {:ints vars :n-values [_ n-values] :ac [_ ac]} }
           (.atLeastNValues model
                            (->> vars (map coerce-int-var) (into-array IntVar))
                            (coerce-int-var n-values)
                            ac)
;; (s/def ::compile-spec
;;   (s/cat :constraint #{constraint-name}
;;          :args       (s/spec
;;                       (s/cat
;;                        :ints     (s/coll-of int-var?)
;;                        :n-values (s/spec (s/tuple #{'n-values} int-var?))
;;                        :ac       (s/spec (s/tuple #{'ac} boolean?))))))

;; (defn- compiler [model vars-index statement]
;;   (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))]
;;     (match (->> var-subed-statement (s/conform ::compile-spec))
;;            {:args {:ints vars :n-values [_ n-values] :ac [_ ac]} }
;;            (.atLeastNValues model
;;                             (into-array IntVar vars)
;;                             n-values
;;                             ac)

;;            ::s/invalid
;;            (utils/report-spec-error constraint-name ::compile-spec var-subed-statement))))

(defloco $at-least-n-values
  "Creates an atLeastNValue constraint.
  Let N be the number of distinct values assigned to the variables of the vars collection.
  Enforce condition N >= nValues to hold.
  AC is false by default."
  {:choco "atLeastNValues(IntVar[] vars, IntVar nValues, boolean AC)"}
  ([vars n-values]
   ($at-least-n-values vars n-values false))

  ([vars n-values ac?]
   {:pre [(sequential? vars) (boolean? ac?)]}
   (constraint constraint-name
               [(vec vars)
                ['n-values n-values]
                ['ac ac?]]
               compiler)))
;; (defn $at-least-n-values
;;   "Creates an atLeastNValue constraint.
;;   Let N be the number of distinct values assigned to the variables of the vars collection.
;;   Enforce condition N >= nValues to hold."
;;   {:choco "atLeastNValues(IntVar[] vars, IntVar nValues, boolean AC)"}
;;   ([vars n-values]
;;    (at-least-n-values vars n-values false))

;;   ([vars n-values ac]
;;    {:pre [(sequential? vars) (boolean? ac)]}
;;    (constraint constraint-name
;;                [(vec vars)
;;                 ['n-values n-values]
;;                 ['ac ac]]
;;                compiler)))
