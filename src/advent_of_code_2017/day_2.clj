(ns advent-of-code-2017.day-2)

(require '[clojure.data.csv :as csv]
         '[clojure.java.io :as io])


;; --- Day 2: Corruption Checksum ---
;; As you walk through the door, a glowing humanoid shape yells in
;; your direction. "You there! Your state appears to be idle. Come
;; help us repair the corruption in this spreadsheet - if we take
;; another millisecond, we'll have to display an hourglass cursor!"

;; The spreadsheet consists of rows of apparently-random numbers. To
;; make sure the recovery process is on the right track, they need you
;; to calculate the spreadsheet's checksum. For each row, determine
;; the difference between the largest value and the smallest value;
;; the checksum is the sum of all of these differences.

;; What is the checksum for the spreadsheet in your puzzle input?

(def input
  (with-open [reader (io/reader "resources/day-2/input.csv")]
    (doall
     (csv/read-csv reader :separator \tab))))

(defn corruption-checksum
  ""
  [csv]
  (let [nums (map
              (partial map #(Integer/parseInt %))
              csv)
        maxes (map (partial apply max) nums)
        mins (map (partial apply min) nums)
        diffs (map - maxes mins)]
    (apply + diffs)))
