(ns ptonner.mini-maniac-test
  (:import [ai.djl.nn Parameter])
  (:require [clojure.test :refer :all]
            [clj-djl.ndarray :as nd]
            [ptonner.maniac.mini-maniac :refer :all :as mm]))

(deftest parameter-test
  (testing "Basic parameter creation"
    (let [mgr (nd/new-base-manager)]
      (with-context
        (with-interceptor (message-value)
          (let [p (param ::tmp (nd/zeros mgr [10 10]))]
            (is (instance? Parameter p))
            (is (= p (-> context deref (get-param ::tmp)))))))))
  (testing "Repeat creation re-uses existing parameter"
    (let [mgr (nd/new-base-manager)]
      (with-context
        (with-interceptor (message-value)
          (let [p1 (param ::tmp (nd/zeros mgr [10 10]))
                p2 (param ::tmp (nd/ones mgr [10 10]))]
            (is (= p1 p2))))))))

(comment
  (deftest replay-test
    (testing "Replay uses value from guide trace"
      (is (= 0 (with-context
                 (with-interceptor (trace :guide)
                   (with-interceptor (message-value)
                     (sample :a identity [0])))
                 (with-interceptor (replay :guide)
                   (with-interceptor (message-value)
                     (sample :a identity [1])))))))))
