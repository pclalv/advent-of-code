(ns advent-of-code-2017.day-6-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2017.day-6 :refer :all]))

(deftest redistribute-memory-test
  (testing "output"
    (is (= 5 (redistribute-memory (read-memory "resources/day-6/input-test"))))
    (is (= 4074 (redistribute-memory (read-memory "resources/day-6/input"))))))
