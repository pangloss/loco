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
