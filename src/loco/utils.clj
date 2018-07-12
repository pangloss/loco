(ns loco.utils
  (:refer-clojure :exclude [var?])
  (:require [clojure.core.match :refer [match]]))

(def p partial)
(def c comp)

(defn debug-print [prefix list]
  (match
   [prefix list]
   [(aprefix :guard #(or (symbol? %) (keyword? %) (string? %))) alist]
   (do
     (doseq [item alist]
       (println aprefix item (meta item)))
     alist)

   [alist (aprefix :guard #(or (symbol? %) (keyword? %) (string? %)))]
   (do
     (doseq [item alist]
       (println aprefix item (meta item)))
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

(defn reverse-map [map]
  (reduce (fn [acc [key val]]
            (assoc acc val key)) {} map))

(defn split [pred coll]
  [
   (filter pred coll)
   (remove pred coll)
   ]
  )

;; (defn var?
;;   [form]
;;   (match form
;;          [:var & _] true
;;          :else false))

(defn public-var? [form]
  (match form
         [:var _ :public & _] true
         :else false))

(defn hidden-var? [form]
  (match form
         [:var _ :hidden & _] true
         :else false))

;; (defn proto-var? [form]
;;   (match form
;;          [:var _ :proto & _] true
;;          :else false))

(def var? (c some? :var meta))

(def proto? (c some? :proto meta))

(def constraint? (c some? :constraint meta))

(def partial-constraint? (c some? :partial-constraint meta))

(def view? (c some? :view meta))

(defn reify? [form]
  (match form
         [:reify _ _] true
         :else false))
