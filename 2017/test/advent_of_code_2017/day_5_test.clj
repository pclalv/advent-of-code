(ns advent-of-code-2017.day-5-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2017.day-5 :refer :all]))

(deftest compute-file-test
  (testing "output"
    (is (= 5 (compute-file "resources/day-5/input-test")))
    (is (= 351282 (compute-file "resources/day-5/input")))
    (is (= 10 (compute-file "resources/day-5/input-test" stranger-instruction-modifier)))
    (is (= 24568703 (compute-file "resources/day-5/input" stranger-instruction-modifier)))))
