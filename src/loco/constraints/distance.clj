(ns loco.constraints.distance
  (:require
   [clojure.core.match :refer [match]]
   [clojure.set :as set]
   [clojure.spec.alpha :as s]
   [clojure.walk :as walk]
   [loco.constraints.utils :refer :all :as utils]
   ))

(def ^:private constraint-name 'distance)

(defn- allowed->op [op]
  (-> {= '=
       < '<
       > '>
       not= '!=}
      (get ,, op op)
      name
      symbol))

(def ^:private allowed-op? #{'= '< '> '!=})

(s/def ::compile-spec
  (s/cat :constraint #{constraint-name}
         :args       (s/spec
                      (s/or
                       :int (s/tuple
                             #{'|} ::utils/coerce-intvar? #{'-} ::utils/coerce-intvar? #{'|}
                             allowed-op?
                             int?)
                       :int-var (s/tuple
                                 #{'|} ::utils/coerce-intvar? #{'-} ::utils/coerce-intvar? #{'|}
                                 (set/difference allowed-op? #{'!=})
                                 ::utils/coerce-intvar?)))))

(defn- compiler [model vars-index statement]
  (let [var-subed-statement (->> statement (walk/prewalk-replace vars-index))
        coerce-var (utils/coerce-var model)]
    (match (->> var-subed-statement (s/conform ::compile-spec))
           {:args [:int-var [_ var1 _ var2 _ op eq-var]]}
           (.distance model
                      (coerce-var var1)
                      (coerce-var var2)
                      (str op)
                      (coerce-var eq-var))

           {:args [:int [_ var1 _ var2 _ op eq-var]]}
           (.distance model
                      (coerce-var var1)
                      (coerce-var var2)
                      (str op)
                      eq-var)

           ::s/invalid
           (report-spec-error constraint-name ::compile-spec var-subed-statement))))

;;TODO: this looks like it would make a good partial, however the LH RH stuff is on the wrong sides
;;however a partial would always use the '= op, so the above doesn't matter
(defloco $distance
  "Creates a distance constraint : |var1-var2| op {cste eq-var}
where op can take its value among {=, >, <, !=}

!= is only for int as eq-var
"
  {:choco ["distance(IntVar var1, IntVar var2, String op, int cste)"
           "distance(IntVar var1, IntVar var2, String op, IntVar var3)"]}
  [var1 var2 op eq-var]
  {:pre [((some-fn string? ident? #{= < > not=}) op)
         (if-not (int? eq-var)
           (not (#{not= '!= "!=" :!=} op))
           true)]}
  (constraint constraint-name
              ['| var1 '- var2 '| (allowed->op op) eq-var]
              compiler)
  )
