(ns ptonner.mini-maniac-test
  (:require [clojure.test :refer :all]
            [ptonner.mini-maniac :refer :all]))

(deftest replay-test
  (testing "Replay uses value from guide trace"
    (is (= 0 (with-context
               (with-interceptor (trace :guide)
                 (with-interceptor (message-value)
                   (sample :a identity [0])))
               (with-interceptor (replay :guide)
                 (with-interceptor (message-value)
                   (sample :a identity [1]))))))))
