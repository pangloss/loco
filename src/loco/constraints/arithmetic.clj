(ns loco.constraints.arithmetic
  (:refer-clojure :exclude [- *])
  (:use loco.utils
        loco.constraints.utils)
  (:require [clojure.core.match :refer [match]]
            [loco.constraints.sum :refer [sum]]
            [loco.constraints.arithm :refer [arithm]]))

;;in clojure these are actually able to tell if the args are sorted...


(defn -
  "Takes a combination of int-vars and numbers, and returns another number/int-var which is constrained
  to equal (x - y - z - ...)"
  {:partial true}
  ([& args]
   (partial-constraint [:- (vec args)])))


(defn *
  "Takes two arguments. One of the arguments can be a number greater than or equal to -1."
  {:partial true}
  [& args]
  (match (vec args)
         [x y] (partial-constraint [:* [x y]])
         [x & more] (partial-constraint [:* [x (apply * more)]])))
