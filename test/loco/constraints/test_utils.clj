(ns loco.constraints.test-utils
  (:require
   [loco.model :as model]
   [loco.compiler :as compiler]))

(defn constraints-strings [input]
  (->> input
       model/compile
       compiler/compile
       :model
       .getCstrs
       (map str)))
