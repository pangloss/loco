(ns loco.utils)

(def p partial)
(def c comp)

(defn debug-print [prefix list]
  (doseq [item list]
    (println prefix item))
  list)

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
