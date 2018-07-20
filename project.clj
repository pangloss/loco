(defproject loco "0.3.1"
  :description "Constraint Programming for Clojure"
  :url "http://github.com/aengelberg/loco"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :plugins [[lein-ancient "0.6.14"]]
  :test-selectors {
                   :model :model
                   :compiler :compiler
                   :solutions :solutions
                   }
  :dependencies [
                 [org.clojure/clojure "1.9.0"]
                 [org.clojure/core.match "0.3.0-alpha5"]
                 [defun "0.3.0-RC1"]
                 [camel-snake-kebab "0.4.0"]
                 [org.choco-solver/choco-solver "4.0.6"]
                 [expound "0.7.1"] ;; better error messages for clojure spec
                 ])
