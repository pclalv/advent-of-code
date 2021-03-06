(ns advent-of-code-2017.day-7-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2017.day-7 :refer :all]))

(deftest find-root-program-test
  (testing "return value"
    (is (= "tknk" (find-root-program "input-test")))
    (is (= "dgoocsw" (find-root-program "input")))))

(deftest balance-tower-test
  (testing "solves test input"
    (is (= 60 (:corrected-weight (balance-tower "input-test")))))
  (testing "solves input"
    (is (= 1275 (:corrected-weight (balance-tower "input")))))
  (testing "does not incorrectly solve real input"
    (is (not (= 1373 (:corrected-weight (balance-tower "input")))))
    (is (not (= 40965 (:corrected-weight (balance-tower "input")))))))
