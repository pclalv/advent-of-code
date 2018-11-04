(ns advent-of-code-2017.day-13-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2017.day-13 :refer :all]))

(deftest severity-test
  (testing "return value"
    (is (= 24 (:score (severity "input-test"))))
    (is (= 2508 (:score (severity "input"))))))

(deftest seconds-to-delay-to-not-get-caught-test
  (testing "return value"
    (is (= 10 (seconds-to-delay-to-not-get-caught "input-test")))))
