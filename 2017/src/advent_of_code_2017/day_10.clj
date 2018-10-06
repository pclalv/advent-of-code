(ns advent-of-code-2017.day-10)

;; --- Day 10: Knot Hash ---

;; You come across some programs that are trying to implement a
;; software emulation of a hash based on knot-tying. The hash these
;; programs are implementing isn't very strong, but you decide to help
;; them anyway. You make a mental note to remind the Elves later not
;; to invent their own cryptographic functions.

;; This hash function simulates tying a knot in a circle of string
;; with 256 marks on it. Based on the input to be hashed, the function
;; repeatedly selects a span of string, brings the ends together, and
;; gives the span a half-twist to reverse the order of the marks
;; within it. After doing this many times, the order of the marks is
;; used to build the resulting hash.

;;   4--5   pinch   4  5           4   1
;;  /    \  5,0,1  / \/ \  twist  / \ / \
;; 3      0  -->  3      0  -->  3   X   0
;;  \    /         \ /\ /         \ / \ /
;;   2--1           2  1           2   5

;; To achieve this, begin with a list of numbers from 0 to 255, a
;; current position which begins at 0 (the first element in the list),
;; a skip size (which starts at 0), and a sequence of lengths (your
;; puzzle input). Then, for each length:

;;   - Reverse the order of that length of elements in the list,
;;     starting with the element at the current position.
;;   - Move the current position forward by that length plus the skip size.
;;   - Increase the skip size by one.

;; The list is circular; if the current position and the length try to
;; reverse elements beyond the end of the list, the operation reverses
;; using as many extra elements as it needs from the front of the
;; list. If the current position moves past the end of the list, it
;; wraps around to the front. Lengths larger than the size of the list
;; are invalid.

;; Here's an example using a smaller list:

;; Suppose we instead only had a circular list containing five
;; elements, 0, 1, 2, 3, 4, and were given input lengths of 3, 4, 1,
;; 5.

;;   - The list begins as ([`0`] `1` `2` `3` `4`) (where square brackets
;;     indicate the current position).
;;   - The first length, `3`, selects ([`0`] `1` `2`) `3` `4` (where
;;     parentheses indicate the sublist to be reversed).
;;   - After reversing that section (`0` `1` `2` into `2` `1` `0`), we
;;     get ([`2`] `1` `0`) `3` `4`.
;;   - Then, the current position moves forward by the length, `3`,
;;     plus the skip size, `0`: `2` `1` `0` [`3`] `4`. Finally, the skip size
;;     increases to `1`.

;;   - The second length, `4`, selects a section which wraps: `2` `1`) `0` ([`3`] `4`.
;;   - The sublist `3` `4` `2` `1` is reversed to form `1` `2` `4` `3`: `4` `3`) `0` ([`1`] `2`.
;;   - The current position moves forward by the length plus the skip
;;     size, a total of `5`, causing it not to move because it wraps
;;     around: `4` `3` `0` [`1`] `2`. The skip size increases to `2`.

;;   - The third length, `1`, selects a sublist of a single element,
;;     and so reversing it has no effect.
;;   - The current position moves forward by the length (`1`) plus the
;;     skip size (`2`): `4` [`3`] `0` `1` `2`. The skip size increases
;;     to `3`.

;;   - The fourth length, `5`, selects every element starting with the
;;     second: `4`) ([`3`] `0` `1` `2`.
;;     Reversing this sublist (`3`)) `0` `1` `2` `4` into
;;     `4` `2` `1` `0` `3`) produces: `3` ([`4`]) `2 `1` `0`.
;;   - Finally, the current position moves forward by `8`:
;;     `3` `4` `2` `1` [`0`]. The skip size increases to `4`.

;; In this example, the first two numbers in the list end up being `3`
;; and `4`; to check the process, you can multiply them together to
;; produce `12`.

;; However, you should instead use the standard list size of
;; `256` (with values `0` to `255`) and the sequence of lengths in
;; your puzzle input. Once this process is complete, what is the
;; result of multiplying the first two numbers in the list?

(defn knot-hash
  ([numbers lengths]
   (knot-hash numbers 0 0
              (filter #(>= 256 %) lengths)))
  ([numbers position skip [length & lengths]]
   (if (nil? length)
     numbers
     (let [numbers-cycle (cycle numbers)
           reversed-sublist (->> numbers-cycle
                                 (drop position)
                                 (take length)
                                 (reverse))
           unaffected-sublist (->> numbers-cycle
                                   (drop (+ position length))
                                   (take (- (count numbers) length)))
           new-numbers-cycle (cycle (concat reversed-sublist unaffected-sublist))
           new-numbers (->> new-numbers-cycle
                            (drop (- (count numbers)
                                     position))
                            (take (count numbers)))
           new-position (-> position
                            (+ length skip)
                            (mod (count numbers)))]
       (recur new-numbers new-position (inc skip) lengths)))))

(comment
  (let [nums [2 1 0 3 4]
        nums-cycle (cycle nums)
        position 3
        length 4]
    (eq '(3 4 2 1)
        (->> (nums-cycle
              (drop position)
              (take length))))

    (eq '(0)
        (->> nums-cycle
             (drop (+ position length))
             (take (- (count nums) length)))))

  [2 1 0 3 4] [2 1 0 3 4]
  ;;   v position (drop position) ->> (take (count numbers))
  [1 2 4 3 0] [1 2 4 3 0])

(defn part1 []
  (let [lengths-file-contents (->> "resources/day-10/input"
                                   slurp
                                   clojure.string/trim-newline)
        lengths (->> (clojure.string/split lengths-file-contents #",")
                     (map #(Integer/parseInt %)))
        hash (knot-hash (range 0 256) lengths)]
    (->> hash
         (take 2)
         (reduce *))))

(comment "resulting hash from input"
         '(38 37 36 35 34 33 32 31 29 30
              27 28 26 134 133 132 131 130 129 128 127 126 125 124 123 122
              106 107 108 109 110 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3
              2 1 50 51 52 53 54 55 56 57 93 92 91 90 89 88 87 86 85 84 83
              82 81 80 79 78 77 76 75 74 73 72 71 70 69 60 61 62 63 64 65
              66 67 68 59 58 94 95 96 97 98 179 180 181 182 183 184 185 186
              187 188 189 190 191 192 193 194 195 196 197 198 199 200 201
              202 203 204 205 206 207 208 209 21 20 19 111 112 113 114 115
              116 117 211 212 213 214 215 216 217 218 219 220 221 222 223
              224 225 226 227 228 229 230 231 232 233 234 235 236 237 238
              239 240 241 242 243 244 245 246 247 248 249 250 251 252 253
              254 255 105 104 103 102 101 100 99 178 177 176 175 174 173
              172 171 170 169 168 167 166 165 164 163 162 25 135 136 137
              138 139 140 141 142 143 144 145 146 147 148 149 150 151 152
              153 154 155 156 157 158 159 160 161 24 23 22 210 118 119 120
              121 0 49 48 47 46 45 44 43 42 41 40 39))

