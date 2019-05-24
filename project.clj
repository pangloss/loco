(defproject loco "0.4.1"
  :description "Constraint Programming for Clojure"
  :url "http://github.com/boxxxie/loco"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :test-selectors {
                   :model :model
                   :compiler :compiler
                   :solutions :solutions
                   :loco :loco
                   }
  :dependencies [
                 [org.clojure/clojure "1.10.0"]
                 [org.clojure/core.match "0.3.0"]

                 ;; FIXME: remove defun, was an experiment
                 [defun "0.3.0-RC1"] ;; bad idea
                 [camel-snake-kebab "0.4.0"]
                 [org.choco-solver/choco-solver "4.10.0"]
                 [org.clojure/math.combinatorics "0.1.5"]
                 [expound "0.7.2"] ;; better error messages for clojure spec

                 ;;pprint
                 [fipp "0.6.18"]

                 [com.rpl/specter "1.1.2"]
                 ]
  :jvm-opts [
             "-Djava.library.path=/home/pauli/ibex/ibex/lib/lib"
             ]
  )
