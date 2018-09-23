(ns advent-of-code-2017.day-9-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2017.day-9 :refer :all]))

(deftest count-groups-test
  (testing "return value"
    (is (= 1 (count-groups "{}")))
    (is (= 3 (count-groups "{{{}}}")))
    (is (= 3 (count-groups "{{},{}}")))
    (is (= 6 (count-groups "{{{},{},{{}}}}")))
    (is (= 1 (count-groups "{<{},{},{{}}>}")))
    (is (= 1 (count-groups "{<a>,<a>,<a>,<a>}")))
    (is (= 5 (count-groups "{{<a>},{<a>},{<a>},{<a>}}")))
    (is (= 2 (count-groups "{{<!>},{<!>},{<!>},{<a>}}")))))


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
