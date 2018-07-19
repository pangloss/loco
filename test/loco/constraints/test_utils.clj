(ns loco.constraints.test-utils
  (:require
   [loco.model :as model]
   [loco.compiler :as compiler]
   [clojure.test :refer :all]))

(defn constraints-strings [input]
  (->> input
       model/compile
       compiler/compile
       :model
       .getCstrs
       (map str)))

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
