(ns loco.constraints.cumulative-test
  (:require
   [clojure.test :refer :all]
   [loco.constraints :refer :all]
   [loco.constraints.test-utils :as utils]
   ))

#_(testing "cumulative"
    (is
     (compile-constraint?
      '("CUMULATIVE ([PropCumulative([cste -- 0 = 0,cste -- 1 = 1,cste -- 1 = 1,1 = 1],capacity = {0..10}), PropCumulative([cste -- 0 = 0,cste -- 1 = 1,cste -- 1 = 1,1 = 1],capacity = {0..10})])")
      [($task :task1 [0] [1] [1])
       ($int :capacity 0 10)
       ($cumulative [:task1] [1] :capacity)
       ]
      ))

    (is
     (compile-constraint?
      '("CUMULATIVE ([PropGraphCumulative([cste -- 0 = 0,cste -- 1 = 1,cste -- 1 = 1,1 = 1],capacity = {0..10}), PropGraphCumulative([cste -- 0 = 0,cste -- 1 = 1,cste -- 1 = 1,1 = 1],capacity = {0..10})])")
      [($task :task1 [0] [1] [1])
       ($int :capacity 0 10)
       ($cumulative [:task1] [1] :capacity true [:time :nrj :sweep])
       ]
      ))
    )

(deftest cumulative-test

  ;;example from GCCAT
  ;;http://sofdem.github.io/gccat/gccat/Ccumulative.html
  (is
   (loco?
    [
     ($task :task1 1 3 4) ;;start duration end
     ($task :task2 2 9 11)
     ($task :task3 3 10 13)
     ($task :task4 6 6 12)
     ($task :task5 7 2 9)
     ($cumulative {:task1 1
                   :task2 2
                   :task3 1
                   :task4 1
                   :task5 3}
                  :capacity 8)]
    {:solutions
     #{{:task1 {:start 1, :duration 3, :end 4},
        :task2 {:start 2, :duration 9, :end 11},
        :task3 {:start 3, :duration 10, :end 13},
        :task4 {:start 6, :duration 6, :end 12},
        :task5 {:start 7, :duration 2, :end 9}}}}
    ))

  (is
   (loco?
    [
     ($task :task1 1 3 4) ;;start duration end
     ($task :task2 2 9 11)
     ($task :task3 3 10 13)
     ($task :task4 6 6 12)
     ($task :task5 7 2 9)
     ($in :capacity 3 5)
     ($cumulative {:task1 1
                   :task2 2
                   :task3 1
                   :task4 1
                   :task5 3}
                  :capacity :capacity)]
    {:solutions
     #{{:task1 {:start 1, :duration 3, :end 4},
        :task2 {:start 2, :duration 9, :end 11},
        :task3 {:start 3, :duration 10, :end 13},
        :task4 {:start 6, :duration 6, :end 12},
        :task5 {:start 7, :duration 2, :end 9},
        :capacity 4}
       {:task1 {:start 1, :duration 3, :end 4},
        :task2 {:start 2, :duration 9, :end 11},
        :task3 {:start 3, :duration 10, :end 13},
        :task4 {:start 6, :duration 6, :end 12},
        :task5 {:start 7, :duration 2, :end 9},
        :capacity 3}
       {:task1 {:start 1, :duration 3, :end 4},
        :task2 {:start 2, :duration 9, :end 11},
        :task3 {:start 3, :duration 10, :end 13},
        :task4 {:start 6, :duration 6, :end 12},
        :task5 {:start 7, :duration 2, :end 9},
        :capacity 5}}}
    ))

  )
