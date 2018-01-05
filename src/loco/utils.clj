(ns loco.utils)

(def p partial)
(def c comp)

(defn debug-print [prefix list]
  (doseq [item list]
    (println prefix item))
  list)
