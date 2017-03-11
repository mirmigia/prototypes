(ns universe-2d.core-test
  (:require [clojure.test :refer :all]
            [universe-2d.core :refer :all]))

(deftest vector-manipulation
  (testing "addition"
    (is (= (v+ [10 20] [20 10]) [30 30]))
    (is (= (v+ [10 20] [30 40] [50 60]) [90 120])))
  (testing "subtraction"
    (is (= (v- [30 20] [20 10]) [10 10]))
    (is (= (v- [100 200] [30 40] [50 60]) [20 100])))
  (testing "multiplication"
    (is (= (v* [3 2] [2 1]) [6 2]))
    (is (= (v* [10 2] [3 5] [10 3]) [300 30])))
  ;; TODO: Test division by zero
  (testing "division"
    (is (= (v% [20 7] [2 2]) [10 3.5]))
    (is (= (v% [200 30] [10 3] [10 2]) [2 5]))))
