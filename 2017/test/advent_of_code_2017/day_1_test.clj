(ns advent-of-code-2017.day-1-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2017.day-1 :refer :all]))

(deftest sum-matches-test
  (testing "output"
    (is (= 3 (sum-matches "1122")))
    (is (= 4 (sum-matches "1111")))
    (is (= 0 (sum-matches "1234")))
    (is (= 9 (sum-matches "91212129")))))

(deftest sum-around-matches-test
  (testing "output"
    (is (= 6 (sum-matches-around "1212")))
    (is (= 0 (sum-matches-around "1221")))
    (is (= 4 (sum-matches-around "123425")))
    (is (= 4 (sum-matches-around "12131415")))
    (is (= 12 (sum-matches-around "123123")))
    (is (= 20 (sum-matches-around "12341234")))))
