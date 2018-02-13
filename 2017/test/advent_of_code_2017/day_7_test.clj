(ns advent-of-code-2017.day-7-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2017.day-7 :refer :all]))

(deftest find-root-program-test
  (testing "output"
    (is (= "tknk" (find-root-program "resources/day-7/input-test")))))
