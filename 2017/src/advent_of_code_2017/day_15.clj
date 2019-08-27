(ns advent-of-code-2017.day-15)
(require '[clojure.pprint :refer (cl-format)])

;; --- Day 15: Dueling Generators ---

;; Here, you encounter a pair of dueling generators. The generators,
;; called generator A and generator B, are trying to agree on a
;; sequence of numbers. However, one of them is malfunctioning, and so
;; the sequences don't always match.

;; As they do this, a judge waits for each of them to generate its
;; next value, compares the lowest 16 bits of both values, and keeps
;; track of the number of times those parts of the values match.

;; The generators both work on the same principle. To create its next
;; value, a generator will take the previous value it produced,
;; multiply it by a factor (generator A uses 16807); generator B uses
;; 48271), and then keep the remainder of dividing that resulting
;; product by 2147483647. That final remainder is the value it
;; produces next.

;; To calculate each generator's first value, it instead uses a
;; specific starting value as its "previous value" (as listed in your
;; puzzle input).

;; For example, suppose that for starting values, generator A uses 65,
;; while generator B uses 8921. Then, the first five pairs of
;; generated values are:

;; --Gen. A--  --Gen. B--
;;    1092455   430625591
;; 1181022009  1233683848
;;  245556042  1431495498
;; 1744312007   137874439
;; 1352636452   285222916

;; In binary, these pairs are (with generator A's value first in each
;; pair):

;; 00000000000100001010101101100111
;; 00011001101010101101001100110111

;; 01000110011001001111011100111001
;; 01001001100010001000010110001000

;; 00001110101000101110001101001010
;; 01010101010100101110001101001010

;; 01100111111110000001011011000111
;; 00001000001101111100110000000111

;; 01010000100111111001100000100100
;; 00010001000000000010100000000100

;; Here, you can see that the lowest (here, rightmost) 16 bits of the
;; third value match: 1110001101001010. Because of this one match,
;; after processing these five pairs, the judge would have added only
;; 1 to its total.

;; To get a significant sample, the judge would like to consider 40
;; million pairs. (In the example above, the judge would eventually
;; find a total of 588 pairs that match in their lowest 16 bits.)

;; After 40 million pairs, what is the judge's final count?

;; Generator A starts with 277
;; Generator B starts with 349
(def input {:a-initial 277 :b-initial 349})
(def test-input {:a-initial 65 :b-initial 8921})

(def generator-a-factor 16807)
(def generator-b-factor 48271)
(def divisor 2147483647)

(defn generator-a [initial-value] (iterate (fn [value]
                                             (rem (* value generator-a-factor)
                                                  divisor))
                                           initial-value))

(defn generator-b [initial-value] (iterate (fn [value]
                                             (rem (* value generator-b-factor)
                                                  divisor))
                                           initial-value))

(defn next-16-bits [generator]
  (->> generator
       (take 1)
       first
       (cl-format nil "~32,'0',B")
       (drop 16)
       (apply str)))

(defn compare-generators [{:keys [a-initial b-initial]} max-comparisons]
  (loop [a (rest (generator-a a-initial))
         b (rest (generator-b b-initial))
         tally 0
         comparisons 0]
    (if (> max-comparisons comparisons)
      (let [this-a (next-16-bits a)
            this-b (next-16-bits b) (if (= this-a this-b)
                                      (inc tally)
                                      tally)]
        (recur (rest a) (rest b) next-tally (inc comparisons)))
      tally)))
