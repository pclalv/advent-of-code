(ns advent-of-code-2017.day-8-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2017.day-8 :refer :all]))

(deftest largest-value-in-any-register-test
  (testing "return value"
    (is (= 1 (largest-value-in-any-register "input-test")))
    (is (= 8022 (largest-value-in-any-register "input")))))

(deftest highest-value-in-any-register-ever-test
  (testing "return value"
    (is (= 10 (highest-value-in-any-register-ever "input-test")))
    (is (= 9819 (highest-value-in-any-register-ever "input-test")))))
