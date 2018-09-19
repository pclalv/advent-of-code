(ns advent-of-code-2017.day-8)

;; --- Day 8: I Heard You Like Registers ---
;; You receive a signal directly from the CPU. Because of your recent
;; assistance with jump instructions, it would like you to compute the
;; result of a series of unusual register instructions.

;; Each instruction consists of several parts: the register to modify,
;; whether to increase or decrease that register's value, the amount
;; by which to increase or decrease it, and a condition. If the
;; condition fails, skip the instruction without modifying the
;; register. The registers all start at 0. The instructions look like
;; tnhis:

;; b inc 5 if a > 1
;; a inc 1 if b < 5
;; c dec -10 if a >= 1
;; c inc -20 if c == 10
;; These instructions would be processed as follows:

;; Because a starts at 0, it is not greater than 1, and so b is not modified.
;; a is increased by 1 (to 1) because b is less than 5 (it is 0).
;; c is decreased by -10 (to 10) because a is now greater than or equal to 1 (it is 1).
;; c is increased by -20 (to -10) because c is equal to 10.
;; After this process, the largest value in any register is 1.

;; You might also encounter <= (less than or equal to) or != (not
;; equal to). However, the CPU doesn't have the bandwidth to tell you
;; what all the registers are named, and leaves that to you to
;; determine.

;; What is the largest value in any register after completing the
;; instructions in your puzzle input?

(defn dec [operand amt]
  (- operand amt))

(defn inc [operand amt]
  (+ operand amt))

(defn != [operator amt]
  (not (= operator amt)))

(defn tokenize-instruction-line [instruction-line]
  (let [[operation condition] (clojure.string/split instruction-line #" if ")
        [register operator operand] (clojure.string/split operation #" ")
        [register-to-check comparator target] (clojure.string/split condition #" ")]
    (when (nil? (resolve (symbol comparator)))
      (prn "nil comparator" comparator))
    [register (resolve (symbol operator)) (Integer/parseInt operand)
     register-to-check (resolve (symbol comparator)) (Integer/parseInt target)]))

(defn read-instruction-file [instruction-filename]
  (let [resource-path (str "resources/day-8/" instruction-filename)]
    (with-open [rdr (clojure.java.io/reader resource-path)]
      (doall (->>  rdr
                   (line-seq)
                   (map tokenize-instruction-line))))))

(defn assoc-max [registers _register value]
  (if (> value (get registers :max))
    (assoc registers :max value)
    registers))

(defn process-instruction [process-fn]
  (fn [registers instruction]
    ;; (prn instruction)
    (let [[register operator operand register-to-check comparator target] instruction]
      (if (comparator (get registers register-to-check 0) target)
        ;; process-fn accepts these 3 args
        (process-fn registers
                    register
                    (operator (get registers register 0)
                              operand))
        registers))))

(defn largest-value-in-any-register [instruction-filename]
  (let [registers {}
        instructions (read-instruction-file instruction-filename)]
    (->> instructions
         (reduce (process-instruction assoc) registers)
         (vals)
         (apply max))))

;; --- Part Two ---
;; To be safe, the CPU also needs to know the highest value held in
;; any register during this process so that it can decide how much
;; memory to allocate to these operations. For example, in the above
;; instructions, the highest value ever held was 10 (in register c
;; after the third instruction was evaluated).

(defn highest-value-in-any-register-ever [instruction-filename]
  (let [registers {:max java.lang.Float/NEGATIVE_INFINITY}
        instructions (read-instruction-file instruction-filename)]
    (->> instructions
         (reduce (process-instruction (fn [registers register value]
                                        (-> registers
                                            (assoc register value)
                                            (assoc-max register value))))
                 registers)
         (vals)
         (apply max))))
