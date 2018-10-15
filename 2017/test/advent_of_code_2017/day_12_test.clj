(ns advent-of-code-2017.day-12-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2017.day-12 :refer :all]))

(deftest connections-to-program-test
  (testing "return value"
    (is (= '(0 2 3 4 5 6) (connections-to-zero "input-test")))
    (is (= '(0 1 31 38 55 100 120 122 164 169 181 192 243 256 269 271
             277 303 313 319 331 336 345 398 410 415 453 466 482 498
             531 535 546 553 576 579 584 601 626 647 653 661 664 698
             727 750 757 769 830 904 912 916 936 986 1005 1018 1046
             1056 1073 1082 1092 1093 1099 1132 1134 1146 1169 1174
             1178 1199 1211 1242 1250 1275 1280 1295 1346 1348 1352
             1408 1413 1417 1422 1436 1444 1471 1475 1484 1486 1527
             1530 1534 1537 1538 1571 1582 1619 1683 1755 1756 1757
             1799 1809 1811 1830 1845 1851 1887 1921 1927 1952 1966
             1998)
           (connections-to-zero "input")))
    (is (= '(1 8) (connections-to-program (read-program-pipe-file "input-test2") 1)))))

(deftest groups-test
  (testing "return value"
    (is (= {0 '(0 2 3 4 5 6)
            1 '(1)}
           (groups "input-test")))
    (is (not= {0 '(0 2 3 4 5 6)
               1 '(1)
               8 '(1 8)}
              (groups "input-test2")))
    (is (= {0 '(0 2 3 4 5 6)
            1 '(1 8)}
           (groups "input-test2")))))
