(ns loco.model.test
  (:require
   [loco.model :as model]
   [loco.compiler :as compiler])
  (:use clojure.test))

;;TODO: use clojure.test/assert-expr multi-method instead of custom macros

;;TODO: replace with (are ...) statements
(defmacro compiled-assert
  "used for testing model/compile"
  ([expected model-input msg]
   `(is
     (=
      ~expected
      (model/compile ~model-input))
     ~msg))

  ([expected model-input]
   `(compiled-assert ~expected ~model-input nil)))

(defmacro constraints-thrown-with-msg?
  "used for testing compile chain model/compile -> compiler/compile
  tests the constraints throw"
  ([expected-class expected-msg actual-input]
   `(constraints-thrown-with-msg? ~expected-class ~expected-msg ~actual-input nil))
  ([expected-class expected-msg actual-input msg]
   `(is
     (thrown-with-msg?
      ~expected-class
      ~expected-msg
      (->> ~actual-input
           model/compile
           compiler/compile))
     ~msg)))

(defmacro constraints-thrown?
  "used for testing compile chain model/compile -> compiler/compile
  tests the constraints throw"
  ([expected-class actual-input]
   `(constraints-thrown? ~expected-class ~actual-input nil))
  ([expected-class actual-input msg]
   `(is
     ('thrown?
      ~expected-class
      (->> ~actual-input
           model/compile
           compiler/compile))
     ~msg)))

(defmacro constraints-assert
  "used for testing compile chain model/compile -> compiler/compile
  tests the constraints toStrings in built Model"
  ([expected actual-input] `(constraints-assert ~expected ~actual-input nil))
  ([expected actual-input msg]
   `(is
     (=
      ~expected
      (->> ~actual-input
           model/compile
           compiler/compile
           :model
           .getCstrs
           (map str)
           ))
     ~msg)))

(defmethod assert-expr 'compile-constraint? [msg form]
  `(let [
         input# ~(nth form 2)
         expected# ~(nth form 1)
         actual# (->> input#
                      model/compile
                      compiler/compile
                      :model
                      (.getCstrs)
                      (map (memfn toString)))
         result# (= expected# actual#)
         ]
     (if result#
       (do-report {:type :pass,:message ~msg,:expected '~form,:actual actual#})
       (do-report {:type :fail,:message ~msg,:expected '~form,:actual actual#}))
     result#))
