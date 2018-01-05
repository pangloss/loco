(ns loco.model.test
  (:require [loco.model :as model])
  (:use clojure.test))

;;order of ast statements is very important... in the ast building process, not really after
(defn compiled-assert
  ([expected model-input msg]
   (is
    (=
     expected
     (model/compile model-input))
    msg))

  ([expected model-input]
   (is
    (=
     expected
     (model/compile model-input)))))
