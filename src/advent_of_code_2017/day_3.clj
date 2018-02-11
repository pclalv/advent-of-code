(ns advent-of-code-2017.day-3)

(require '[clojure.math.numeric-tower :as math])

;; --- Day 3: Spiral Memory ---
;; You come across an experimental new kind of memory stored on an infinite two-dimensional grid.

;; Each square on the grid is allocated in a spiral pattern starting at a location marked 1 and then counting up while spiraling outward. For example, the first few squares are allocated like this:

;; 17  16  15  14  13
;; 18   5   4   3  12
;; 19   6   1   2  11
;; 20   7   8   9  10
;; 21  22  23---> ...
;; While this is very space-efficient (no squares are skipped), requested data must be carried back to square 1 (the location of the only access port for this memory system) by programs that can only move up, down, left, or right. They always take the shortest path: the Manhattan Distance between the location of the data and square 1.

;; For example:

;; Data from square 1 is carried 0 steps, since it's at the access port.
;; Data from square 12 is carried 3 steps, such as: down, left, left.
;; Data from square 23 is carried only 2 steps: up twice.
;; Data from square 1024 must be carried 31 steps.
;; How many steps are required to carry the data from the square identified in your puzzle input all the way to the access port?

;; 37  36  35  34  33  32  31
;; 38  17  16  15  14  13  30
;; 39  18   5   4   3  12  29
;; 40  19   6   1   2  11  28
;; 41  20   7   8   9  10  27
;; 42  21  22  23  24  25  26
;; 43  44  45  46  46  48  49

(def input 361527)

(defn next-greatest-odd-square
  [num]
  (let [root (int (math/ceil (math/sqrt num)))
        next (if (odd? root) root (+ 1 root))]
    (int (math/expt next 2))))

(defn corners
  [n]
  (let [square (math/expt n 2)
        side-length (- n 1)]
    (->> square
         (iterate #(- % side-length))
         (take 5))))

(defn contains-num?
  [num [upper lower]]
  (and (>= upper num)
       (<= lower num)))

(defn manhattan-distance
  [num]
  (let [grid-area (next-greatest-odd-square num)
        grid-dim (math/sqrt grid-area)
        grid-corners (corners grid-dim)
        corner-pairs (partition 2 1 grid-corners)
        [upper lower] (first (filter (partial contains-num? num)
                                     corner-pairs))
        distance-from-corner (min (- upper num) (- num lower))]
    (- grid-dim 1 distance-from-corner)))

;; --- Part Two ---
;; As a stress test on the system, the programs here clear the grid
;; and then store the value 1 in square 1. Then, in the same
;; allocation order as shown above, they store the sum of the values
;; in all adjacent squares, including diagonals.

;; So, the first few squares' values are chosen as follows:

;; Square 1 starts with the value 1.

;; Square 2 has only one adjacent filled square (with value 1), so it
;; also stores 1.

;; Square 3 has both of the above squares as neighbors and stores the
;; sum of their values, 2.

;; Square 4 has all three of the aforementioned squares as neighbors
;; and stores the sum of their values, 4.

;; Square 5 only has the first and fourth squares as neighbors, so it
;; gets the value 5.

;; Once a square is written, its value does not change. Therefore, the
;; first few squares would receive the following values:

;; 147  142  133  122   59
;; 304    5    4    2   57
;; 330   10    1    1   54
;; 351   11   23   25   26
;; 362  747  806--->   ...

;; What is the first value written that is larger than your puzzle
;; input?

[1 1 2 4 5 10 11]
[0 1 2 3 3  3  2]

[[1]]

[[1 1]]

[[, ,]
 [1 1]]

[[, 2]
 [1 1]]

[[4 2]
 [1 1]]

[[5 4 2]
 [1 1]]

[[5 4 2]
 [, 1 1]
 [, , ,]]

[[ 5 4  2]
 [10 1  1]
 [,  ,  ,]]

[[ 5 4  2]
 [10 1  1]
 [11 ,  ,]]

[[ 5  4  2]
 [10  1  1]
 [11 23  ,]]

[[ 5  4  2]
 [10  1  1]
 [11 23 25]]

(defn expand-left-and-up
  "start from bottom right and work towards top left by going up and then left"
  [grid]
  (let [grid-size (max (count grid) (count (first grid)))
        right-side (map last (reverse grid))
        first-right-side-adjacent-group (take 2 right-side)
        last-right-side-adjacent-group (take-last 2 right-side)
        middle-right-side-adjacent-groups (partition 3 1 right-side)
        all-but-first-right-side-adjacent-groups (conj (reverse middle-right-side-adjacent-groups)
                                                       last-right-side-adjacent-group)
        right-side-adjacent-groups (conj (reverse all-but-first-right-side-adjacent-groups)
                                         first-right-side-adjacent-group)]
    (let [grid-partially-updated (reverse (map (fn [grid-row adjacents]
                                                 (conj grid-row (reduce + adjacents)))
                                               (reverse grid)
                                               right-side-adjacent-groups))
          corrections-unflattened (map-indexed (fn [idx el]
                                                 (let [zeroes (+ 1 idx)
                                                       nonzeroes (- grid-size zeroes)]
                                                   (into [] (concat (repeat zeroes 0)
                                                                    (repeat nonzeroes el)))))
                                               (reverse (drop 1 (map last grid-partially-updated))))
          corrections (reverse (into [] (apply map + corrections-unflattened)))]
      (map (fn [row correction]
             (conj (into [] (butlast row))
                   (+ correction (last row))))
           grid-partially-updated
           corrections))))

[[5   4  2 57]
 [10  1  1 54]
 [11 23 25 26]]

[[11 10  5]
 [23  1  4]
 [25  1  2]
 [26 54 57]]

(defn row-to-column [rotated row]
  (vec (map (fn [rotated-row el]
              (conj rotated-row el))
            rotated
            row)))

(defn rotate-clockwise [matrix]
  (let [rotated (repeat (count (first matrix )) [])]
    (reduce row-to-column
            rotated
            (reverse matrix))))

(loop [matrix [[ 5  4  2]
               [10  1  1]
               [11 23 25]]]
  (let [next-matrix (rotate-clockwise (vec (expand-left-and-up matrix)))]
    (if-let [greater-than-input (first (filter #(< input %) (last next-matrix)))]
      greater-than-input
      (recur next-matrix))))
