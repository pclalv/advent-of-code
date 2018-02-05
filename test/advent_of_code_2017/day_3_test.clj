(ns advent-of-code-2017.day-3-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2017.day-3 :refer :all]))

(deftest manhattan-distance-test
  (testing "output"
    (is (= 0 (manhattan-distance 1)))
    (is (= 3 (manhattan-distance 12)))
    (is (= 2 (manhattan-distance 23)))
    (is (= 31 (manhattan-distance 1024)))))
