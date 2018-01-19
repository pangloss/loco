(ns loco.match
  (:require [clojure.core.match :refer [match]]
            [clojure.walk :as walk]))

(defn guards-to-map
  "transforms a single matching clause (with new :guard syntax) into supported syntax
    e.g.: [sym _] :guard [sym [pred1 pred2]] return"
  ([matcher return] (list matcher return))
  ([matcher _ guards return]
   (let [guards-map (->> guards
                         (partition 2)
                         (mapcat (fn [[term guard]]
                                (if (sequential? term)
                                  (map (fn [term] [term (list term :guard guard)]) term)
                                  [[term (list term :guard guard)]])))
                         (into {}))
         matcher-update (walk/postwalk-replace guards-map matcher)]
     (list
      (if (empty matcher) ;;handle single-expressions
        (into (empty matcher) matcher-update)
        matcher-update)
      return))))

(defn guard-syntax
  "allow for support of 4 arity match syntax
  e.g.: [sym _] :guard [sym [pred1 pred2]] return"
  [match-rows]
  (let [transformed-guards (->> match-rows
                                 (partition 2)
                                 (reduce
                                  (fn [acc matcher-pair]
                                    (let [previous (peek acc)]
                                      (if (and (= 2 (count previous)) (= :guard (second previous)))
                                        (-> acc
                                            pop
                                            (conj (concat previous matcher-pair)))
                                        (conj acc matcher-pair))))
                                  [])
                                 (mapcat (fn [match-tuple]
                                        (apply guards-to-map match-tuple)))
                                 )]
     transformed-guards))


(defmacro match+ [match-on & match-rows]
  `(match ~match-on
          ~@(guard-syntax match-rows)))
