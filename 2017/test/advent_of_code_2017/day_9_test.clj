(ns advent-of-code-2017.day-9-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2017.day-9 :refer :all]))

(deftest score-test
  (testing "return value"
    (is (= 1 (score "{}")))
    (is (= 6 (score "{{{}}}")))
    (is (= 5 (score "{{},{}}")))
    (is (= 16 (score "{{{},{},{{}}}}")))
    (is (= 1 (score "{<a>,<a>,<a>,<a>}")))
    (is (= 9 (score "{{<ab>},{<ab>},{<ab>},{<ab>}}")))
    (is (= 9 (score "{{<!!>},{<!!>},{<!!>},{<!!>}}")))
    (is (= 3 (score "{{<a!>},{<a!>},{<a!>},{<ab>}}")))))

(deftest count-garbage-test
  (testing "return value"
    (is (= 5601 (count-garbage-filename "input")))))
