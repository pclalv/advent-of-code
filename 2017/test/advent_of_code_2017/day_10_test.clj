(ns advent-of-code-2017.day-10-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2017.day-10 :refer :all]))

(deftest knot-hash-test
  (testing "return value"
    (is (= '(3 4 2 1 0) (:numbers (knot-hash (range 0 5) [3 4 1 5]))))))

(deftest part1-test
  (testing "return value"
    (is (not= 1406 (part1)))
    (is (= 11413 (part1)))))

(deftest hash-string-test
  (testing "return value"
    (is (= "a2582a3a0e66e6e86e3812dcb672a272" (hash-string "")))
    (is (= "33efeb34ea91902bb2f59c9920caa6cd" (hash-string "AoC 2017")))
    (is (= "3efbe78a8d82f29979031a4aa0b16a9d" (hash-string "1,2,3")))
    (is (= "63960835bcdc130f0b66d7ff4f6a5a8e" (hash-string "1,2,4")))))

(deftest part2-test
  (testing "return value"
    (is (= "7adfd64c2a03a4968cf708d1b7fd418d" (part2)))))
