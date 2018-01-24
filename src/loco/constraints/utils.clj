(ns loco.constraints.utils
  (:require [clojure.core.match :refer [match]]))

(defn ^:dynamic preserve-consts [val]
  (match [val (meta val)]
         [_ {:preserve-const true}] val
         [_ {:preserve-consts true}] val
         [(val :guard number?) _] (with-meta [val] {:preserve-const true})
         [(val :guard vector?) _] (with-meta val {:preserve-consts true})
         :else val
         ))

(defn constraint [input]
  {:pre [(vector? input)]}
  [:constraint input])

(defn partial-constraint [input]
  {:pre [(vector? input)]}
  [:constraint :partial input])


(def comparison-operator? (sorted-set := :> :< :!= :>= :<=))
(def arithmetic-operator? (sorted-set :+ :* :/ :-))

(def qualified-comparison-operator? (->>
                                     comparison-operator?
                                     (map #(keyword "op" (name %)))
                                     (into (sorted-set))))

(def qualified-arithmetic-operator? (->>
                                     arithmetic-operator?
                                     (map #(keyword "op" (name %)))
                                     (into (sorted-set))))

(def qualified-operator-map
  (zipmap
   comparison-operator?
   qualified-comparison-operator?))
