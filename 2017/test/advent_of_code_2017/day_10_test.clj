(ns advent-of-code-2017.day-10-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2017.day-10 :refer :all]))

(deftest knot-hash-test
  (testing "return value"
    (is (= '(3 4 2 1 0) (knot-hash (range 0 5) [3 4 1 5])))))

(deftest part1-test
  (testing "return value"
    (is (not= 1406 (part1)))))
