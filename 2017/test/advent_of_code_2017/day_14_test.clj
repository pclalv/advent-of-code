(ns advent-of-code-2017.day-14-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2017.day-14 :refer :all]))

(deftest count-occupied-test
  (testing "return value"
    (is (= 8108 (count-occupied input-test)))
    (is (= 8230 (count-occupied input)))))
