(ns loco.constraints.test-utils
  (:require
   [clojure.test :refer :all]
   [loco.compiler :as compiler]
   [loco.model :as model]
   [loco.solver :as solver]
   ))

(defn compiled-constraints-strings [input]
  (->> input
       model/compile
       compiler/compile
       :model
       .getCstrs
       (map str)))

(defn compiled-vars-strings [input]
  (->> input
       model/compile
       compiler/compile
       :vars
       (map str)))

(defn multi-test [input]
  [input
   (model/compile input)
   (compiled-constraints-strings input)
   (compiled-vars-strings input)
   (solver/solutions input)])

(defmacro choco-vars-string-assert
  "used for testing compile chain model/compile -> compiler/compile
  tests properties of vars in built Model"
  ([expected actual-input] `(choco-vars-string-assert ~expected ~actual-input nil))
  ([expected actual-input msg]
   `(is
     (=
      ~expected
      (->>
       ~actual-input
       model/compile
       compiler/compile
       :vars
       (map str)
       ))
     ~msg)))
