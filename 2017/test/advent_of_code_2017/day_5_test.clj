(ns advent-of-code-2017.day-5-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2017.day-5 :refer :all]))

(deftest compute-file-test
  (testing "output"
    (is (= 5 (compute-file "resources/day-5/input-test")))))
