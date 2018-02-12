(ns advent-of-code-2017.day-6)

(require '[clojure.string :as string])

;; --- Day 6: Memory Reallocation ---
;; A debugger program here is having an issue: it is trying to repair
;; a memory reallocation routine, but it keeps getting stuck in an
;; infinite loop.

;; In this area, there are sixteen memory banks; each memory bank can
;; hold any number of blocks. The goal of the reallocation routine is
;; to balance the blocks between the memory banks.

;; The reallocation routine operates in cycles. In each cycle, it
;; finds the memory bank with the most blocks (ties won by the
;; lowest-numbered memory bank) and redistributes those blocks among
;; the banks. To do this, it removes all of the blocks from the
;; selected bank, then moves to the next (by index) memory bank and
;; inserts one of the blocks. It continues doing this until it runs
;; out of blocks; if it reaches the last memory bank, it wraps around
;; to the first one.

;; The debugger would like to know how many redistributions can be
;; done before a blocks-in-banks configuration is produced that has
;; been seen before.

;; For example, imagine a scenario with only four memory banks:

;; The banks start with 0, 2, 7, and 0 blocks. The third bank has the
;; most blocks, so it is chosen for redistribution.

;; Starting with the next bank (the fourth bank) and then continuing
;; to the first bank, the second bank, and so on, the 7 blocks are
;; spread out over the memory banks. The fourth, first, and second
;; banks get two blocks each, and the third bank gets one back. The
;; final result looks like this: 2 4 1 2.

;; Next, the second bank is chosen because it contains the most
;; blocks (four). Because there are four memory banks, each gets one
;; block. The result is: 3 1 2 3.

;; Now, there is a tie between the first and fourth memory banks, both
;; of which have three blocks. The first bank wins the tie, and its
;; three blocks are distributed evenly over the other three banks,
;; leaving it with none: 0 2 3 4.

;; The fourth bank is chosen, and its four blocks are distributed such
;; that each of the four banks receives one: 1 3 4 1.

;; The third bank is chosen, and the same thing happens: 2 4 1 2.

;; At this point, we've reached a state we've seen before: 2 4 1 2 was
;; already seen. The infinite loop is detected after the fifth block
;; redistribution cycle, and so the answer in this example is 5.

;; Given the initial block counts in your puzzle input, how many
;; redistribution cycles must be completed before a configuration is
;; produced that has been seen before?

(defn read-memory [file]
  (with-open [rdr (clojure.java.io/reader file)]
    (let [banks-raw (first (line-seq rdr))  ;; we only expect there to be one line
          banks (map #(Integer/parseInt %) (string/split banks-raw #"\s"))]
      banks)))

(defn redistribute [banks]
  (let [largest-bank (apply max banks)
        largest-bank-idx (.indexOf banks largest-bank)
        initial (assoc (vec banks) largest-bank-idx 0)
        enough-for-all-banks (int (/ largest-bank (count banks)))
        enough-for-some-banks (mod largest-bank (count banks))
        partially-redistributed (map #(+ enough-for-all-banks %) initial)]
    (loop [position (mod (inc largest-bank-idx) (count banks))
           remaining-undistributed enough-for-some-banks
           redistributed partially-redistributed]
      (if (= 0 remaining-undistributed)
        redistributed
        (recur (mod (inc position) (count banks))
               (dec remaining-undistributed)
               (assoc (vec redistributed)
                      position
                      (inc (nth redistributed position))))))))

(defn redistribute-memory
  ([memory] (redistribute-memory memory [] 0))
  ([memory past-states steps]
   (if (<= 0 (.indexOf past-states memory))
     steps ;; returns number of steps that it took to reach this repeated configuration
     (let [next-state (redistribute memory)]
       (recur next-state
              (conj past-states memory)
              (inc steps))))))

(comment
  (redistribute-memory (read-memory "resources/day-6/input"))
  [1 0 14 14 12 12 10 10 8 8 6 6 4 3 2 1] ;; final state
  (redistribute-memory [1 0 14 14 12 12 10 10 8 8 6 6 4 3 2 1])
  2793) ;; number of steps to reach that state again
