(ns advent-of-code-2017.day-3-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2017.day-3 :refer :all]))

(deftest manhattan-distance-test
  (testing "output"
    (is (= 0 (manhattan-distance 1)))
    (is (= 3 (manhattan-distance 12)))
    (is (= 2 (manhattan-distance 23)))
    (is (= 31 (manhattan-distance 1024)))))

(deftest expand-left-and-up-test
  (testing "expands a square matrix"
    (is (= [[5   4  2 57]
            [10  1  1 54]
            [11 23 25 26] (expand-left-and-up [[ 5  4  2]
                                               [10  1  1]
                                               [11 23 25]])])))
  (testing "expands a rectangular matrix"
    (is (= [[ 5  4  2 57 328]
            [10  1  1 54 354]
            [11 23 25 26 160]] (expand-left-and-up [[5   4  2 57]
                                                    [10  1  1 54]
                                                    [11 23 25 26]])))))

(deftest rotate-clockwise-test
  (testing "rotates a square matrix"
    (is (= [[11 10 5]
            [23 1 4]
            [25 1 2]] (rotate-clockwise [[ 5  4  2]
                                         [10  1  1]
                                         [11 23 25]]))))
  (testing "rotates a rectangular matrix"
    (is (= [[11 10  5]
            [23  1  4]
            [25  1  2]
            [26 54 57] (rotate-clockwise [[ 5  4  2  5]
                                          [10  1  1 54]
                                          [11 23 25 26]])]))))
(deftest part2-soln-test
  (testing "truth"
    (is (= 363010 (part2-soln)))))
