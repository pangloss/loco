(ns loco.constraints.test-utils ;;TODO: move this into different (lower) namespace
  (:require
   [clojure.test :refer :all :as t]
   [loco.compiler :as compiler]
   [loco.model :as model]
   [loco.solver :as solver]
   [loco.utils :refer [p c]]
   ))

(defn compiled-constraints-strings [input] ;;FIXME: deprecated
  (->> input
       model/compile
       compiler/compile
       :model
       .getCstrs
       (map str)))

(defn compiled-vars-strings [input] ;;FIXME: deprecated
  (->> input
       model/compile
       compiler/compile
       :vars
       (map str)))

(defn multi-test [input] ;;FIXME: deprecated
  [input
   (model/compile input)
   (compiled-constraints-strings input)
   (compiled-vars-strings input)
   (solver/solutions input)])

(defmacro choco-vars-string-assert ;;FIXME: deprecated
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

(defmacro test-loco [input expected]
  (let [{:keys [identity model compiled solutions]} expected]
    `(let [
           input#     ~input
           expected#  ~expected
           identity#  ~identity
           model#     ~model
           compiled#  ~compiled
           solutions# ~solutions
           model-fn#     (memoize ~model/compile)
           compile-fn#   (memoize ~compiler/compile)
           solutions-fn# ~solver/solutions
           compiled-strings# (juxt
                              (comp (partial mapv str) :vars)
                              (comp (partial mapv str) (memfn ~'getCstrs) :model))
           ]
       (assert (every? #{:identity :model :compiled :solutions} (keys expected#)))
       (when identity# (is (= identity#
                              input#)
                           "identity"))
       (when model# (is (= model#
                           (model-fn# input#))
                        "model"))
       (when compiled# (is (= compiled#
                              (->> input# model-fn# compile-fn# compiled-strings#))
                           "compiled"))
       (when solutions# (is (= solutions#
                               (->> input# model-fn# compile-fn# solutions-fn# set))
                            "solutions"))       
       )))
