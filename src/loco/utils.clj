(ns loco.utils
  (:require [clojure.core.match :refer [match]]))

(def p partial)
(def c comp)

(defn debug-print [prefix list]
  (match
   [prefix list]
   [(aprefix :guard #(or (symbol? %) (keyword? %) (string? %))) alist]
   (do
     (doseq [item alist]
       (println aprefix item))
     alist)

   [alist (aprefix :guard #(or (symbol? %) (keyword? %) (string? %)))]
   (do
     (doseq [item alist]
       (println aprefix item))
     alist)))

(defn remove-dupes [ast]
  ;; we use a vec to maintain order, and a set for fast lookup of dupes
  (->>
   ast
   (reduce
    (fn [[acc-set acc-vec] statement]
      (if (acc-set statement)
        [acc-set acc-vec]
        [(conj acc-set statement)
         (conj acc-vec statement)]
        )
      )
    [#{} []])
   second))

(defn remove-dupes-by [ast key-fn]
  ;; we use a vec to maintain order, and a set for fast lookup of dupes
  (->>
   ast
   (reduce
    (fn [[acc-set acc-vec] statement]
      (if (acc-set (key-fn statement))
        [acc-set acc-vec]
        [(conj acc-set (key-fn statement))
         (conj acc-vec statement)]
        )
      )
    [#{} []])
   second))

(defn index-by [f coll]
  (->>
   coll
   (map (juxt f identity))
   (into {})))
