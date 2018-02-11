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

;; --- Part Two ---
;; "Great work; looks like we're on the right track after all. Here's a star for your effort." However, the program seems a little worried. Can programs be worried?

;; "Based on what we're seeing, it looks like all the User wanted is some information about the evenly divisible values in the spreadsheet. Unfortunately, none of us are equipped for that kind of calculation - most of us specialize in bitwise operations."

;; It sounds like the goal is to find the only two numbers in each row where one evenly divides the other - that is, where the result of the division operation is a whole number. They would like you to find those numbers on each line, divide them, and add up each line's result.

;; For example, given the following spreadsheet:

;; 5 9 2 8
;; 9 4 7 3
;; 3 8 6 5
;; In the first row, the only two numbers that evenly divide are 8 and 2; the result of this division is 4.
;; In the second row, the two numbers are 9 and 3; the result is 3.
;; In the third row, the result is 2.
;; In this example, the sum of the results would be 4 + 3 + 2 = 9.

;; What is the sum of each row's result in your puzzle input?

(defn divisible-pair
  "expects"
  [nums]
  (let [sorted-nums (sort nums)]
    (loop [least (first sorted-nums)
           tail (rest sorted-nums)]
      (if-let [dividend (first (filter #(= 0 (mod % least)) tail))]
        [dividend least]
        (recur (first tail)
               (rest tail))))))

(defn corruption-checksum-2
  [csv]
  (let [nums (map
              (partial map #(Integer/parseInt %))
              csv)
        divisible-pairs (map divisible-pair nums)
        quotients (map (partial apply /) divisible-pairs)]
    (apply + quotients)))
