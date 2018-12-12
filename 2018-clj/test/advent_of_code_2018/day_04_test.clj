(ns advent-of-code-2018.day-04-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2018.day-04 :refer :all]))

(deftest part1-test
  (testing "return value"
    (is (= 240 (part1 "input-test")))
    (is (= 14346 (part1 "input")))))
