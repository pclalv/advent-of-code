(ns advent-of-code-2017.day-7-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2017.day-7 :refer :all]))

(deftest find-root-program-test
  (testing "return value"
    (is (= "tknk" (find-root-program "input-test")))
    (is (= "dgoocsw" (find-root-program "input")))))

(deftest balance-tower-test
  (testing "return value"
    (is (= 60 (:correct-weight (balance-tower "input-test"))))))
    ;(is (= "dgoocsw" (find-root-program "input")))))

