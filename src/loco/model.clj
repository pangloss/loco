(ns loco.model)

(defn- intersect-domains
  [d1 d2]
  (cond
    (and (not (map? d1))
         (not (map? d2))) (filter (set d1) d2)
    (and (not (map? d1))
         (map? d2)) (let [{lo :min hi :max} d2]
                      (filter #(<= lo % hi) d1))
    (and (map? d1)
         (not (map? d2))) (recur d2 d1)
    :else (let [{lo1 :min hi1 :max b1? :bounded} d1
                {lo2 :min hi2 :max b2? :bounded} d2
                m {:min (max lo1 lo2)
                   :max (min hi1 hi2)}]
            (if (and b1? b2?)
              (assoc m :bounded true)
              m))))

(defn- top-level-var-declarations
  "finds top-level domain declarations, merges them per-variable, and
  returns a list of variable declarations"
  ([name-seq data]
   (let [
         domain-decls (filter :can-init-var data)
         all-domains (group-by :name domain-decls)
         ]
     (->>
      [all-domains name-seq]
      (apply map (fn [[var-name decls] rand-name]
             (let [
                   final-domain (reduce intersect-domains (map :domain decls))
                   ]
               (if (if (map? final-domain)
                     (= final-domain {:min 0 :max 1})
                     (= #{0 1} (set final-domain)))
                 {:type :bool-var
                  :name var-name
                  :real-name (str "bool-" rand-name)}
                 {:type :int-var
                  :name var-name
                  :real-name (str "int-" rand-name)
                  :domain (reduce intersect-domains (map :domain decls))}))))))))

(defn- without-top-level-var-declarations
  [data]
  (remove :can-init-var data))

(defn translate
  "take in a representation of a model, a list of maps created using the
  constraints namespace. Transform into a model that can be consumed
  by model/realize, which creates a choco/Model object"
  ([problem] (translate
              ;; this is to maintain compatibility with old code (if
              ;; it is not needed anymore with 4.0.0 update, then this
              ;; should be removed
              (repeatedly (comp name (partial gensym "var")))
              problem))

  ([name-seq problem]
   (concat
    ;; dig for the var declarations and put them at the front
    (top-level-var-declarations name-seq problem)
    (without-top-level-var-declarations problem))))

(defn realize
  "uses output from translate to create a Choco Model object for use
  with a Choco Solver"
  [translated-model])
