(ns advent-of-code-2017.day-11-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2017.day-11 :refer :all]))

(deftest reduce-steps-test
  (testing "return value"
    (is (= [:ne :ne :ne] (reduce-steps [:ne :ne :ne])))
    (is (= [] (reduce-steps [:ne :ne :sw :sw])))
    (is (= [:se :se] (reduce-steps [:ne :ne :s :s])))
    (is (= [:s :s :sw] (reduce-steps [:s :s :sw])))))

(deftest part1-test
  (testing "return value"
    (is (not= 8223 (part1)))))
