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
