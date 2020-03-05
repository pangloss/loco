;; FIXME: implement nth
(ns loco.constraints.nth
  (:require
   [loco.constraints.utils :refer :all]
   [meander.epsilon :as m :refer [match]]))

(def ^:private partial-name 'nth)

(defn- name-fn [body]
  (match body
    [?table
     [at ?index]
     [offset ?offset]]
    (apply str (interpose "_" ?table) "_"
           (when (pos? ?index)
             (str 'index "=" ?index))
           (when (pos? ?offset)
             (str 'offset "=" ?offset)))))

;;TODO: transformation fn
;; [var-name [:$nth [vars [:at index] [:offset offset]]]]
;; (-> []
;;     (into [statement])
;;     (into [($element var-name vars index offset)]))

;;TODO: this may be redundant
;; (defn- deps-fn [body]
;;   (first body))

;; [[:$nth [_ _ [:offset (offset :guard nat-int?)]]] [[:int index-lb index-ub] & vars]]
;; (into [:int] (element-domains vars index-lb index-ub offset))

;;should be given fully expanded vars [:var _ _ [:int _ _ ]]
;;FIXME: finish element-domains for nth
(defn- element-domains
  [body]
  (match body [?table
               [at ?index]
               [offset  ?offset]] nil)
  #_(->>
   deps
   lb-ub-seq
   (drop offset)
   (drop idx-lb)
   (take (inc idx-ub))
   ((juxt (comp first first (p sort-by first))
          (comp last last (p sort-by second))))))

;;FIXME: $nth needs implementing
(defn $nth
  "partial for $element"
  {:choco "element(IntVar value, IntVar[] table, IntVar index, int offset)"
   :partial true}
  ([vars index]
   (nth vars index 0))

  ([vars index offset]
   {:pre [(nat-int? offset) (sequential? vars)]}
   (let [table (if (every? int? vars)
                  (vec vars)
                 (vec vars))
         body [table
               ['at index]
               ['offset  offset]]
         ]
     (partial-constraint
      partial-name
      body
      name-fn
      ;;calc-domain domain-fn
      ;;:get-deps deps-fn
      )
     ;;(add-deps table) ;; i don't know what add deps is supposed to be >_<
     )))
