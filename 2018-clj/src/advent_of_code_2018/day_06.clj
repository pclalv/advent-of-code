(ns advent-of-code-2018.day-06)

(require '[advent-of-code-2018.util :refer (abs input-file-lines)])

(defn input-file-coords [input-file]
  (let [input-file' (str "day_06/" input-file)]
    (->> input-file'
         input-file-lines
         (map #(clojure.string/split % #", "))
         (map (fn [[p0 p1]]
                [(Integer/parseInt p0)
                 (Integer/parseInt p1)])))))

(defn manhattan-distance [[x0 y0] [x1 y1]]
  (+ (abs (- x0 x1))
     (abs (- y0 y1))))

;; --- Day 6: Chronal Coordinates ---

;; The device on your wrist beeps several times, and once again you
;; feel like you're falling.

;; "Situation critical," the device announces. "Destination
;; indeterminate. Chronal interference detected. Please specify new
;; target coordinates."

;; The device then produces a list of coordinates (your puzzle
;; input). Are they places it thinks are safe or dangerous? It
;; recommends you check manual page 729. The Elves did not give you a
;; manual.

;; If they're dangerous, maybe you can minimize the danger by finding
;; the coordinate that gives the largest distance from the other
;; points.

;; Using only the Manhattan distance, determine the area around each
;; coordinate by counting the number of integer X,Y locations that are
;; closest to that coordinate (and aren't tied in distance to any
;; other coordinate).

;; Your goal is to find the size of the largest area that isn't
;; infinite. For example, consider the following list of coordinates:

;; 1, 1
;; 1, 6
;; 8, 3
;; 3, 4
;; 5, 5
;; 8, 9

;; If we name these coordinates A through F, we can draw them on a
;; grid, putting 0,0 at the top left:

;; ..........
;; .A........
;; ..........
;; ........C.
;; ...D......
;; .....E....
;; .B........
;; ..........
;; ..........
;; ........F.

;; This view is partial - the actual grid extends infinitely in all
;; directions. Using the Manhattan distance, each location's closest
;; coordinate can be determined, shown here in lowercase:

;; aaaaa.cccc
;; aAaaa.cccc
;; aaaddecccc
;; aadddeccCc
;; ..dDdeeccc
;; bb.deEeecc
;; bBb.eeee..
;; bbb.eeefff
;; bbb.eeffff
;; bbb.ffffFf

;; Locations shown as . are equally far from two or more coordinates,
;; and so they don't count as being closest to any.

;; In this example, the areas of coordinates A, B, C, and F are
;; infinite - while not shown here, their areas extend forever outside
;; the visible grid. However, the areas of coordinates D and E are
;; finite: D is closest to 9 locations, and E is closest to 17 (both
;; including the coordinate's location itself). Therefore, in this
;; example, the size of the largest area is 17.

(defn populate-grid-with-closest-destination [destinations grid]
  (reduce (fn [grid' [destination-id destination]]
            (map-indexed (fn [row-idx row]
                           (map-indexed (fn [col-idx current-coord-data]
                                          (let [current-coord [row-idx col-idx]
                                                closest-distance-to-current-coord-so-far (:distance current-coord-data)
                                                current-distance-to-current-coord (manhattan-distance current-coord destination)]
                                            (cond (= current-distance-to-current-coord closest-distance-to-current-coord-so-far)
                                                  {:destination-ids (conj (:destination-ids current-coord-data) destination-id)
                                                   :distance current-distance-to-current-coord}
                                                  (< current-distance-to-current-coord closest-distance-to-current-coord-so-far)
                                                  {:destination-ids [destination-id]
                                                   :distance current-distance-to-current-coord}
                                                  :else current-coord-data)))
                                        row))
                         grid'))
          grid
          destinations))

(defn edge? [grid]
  (let [top-row    (set     (first grid))
        bottom-row (set      (last grid))
        left-row   (set (map first grid))
        right-row  (set (map  last grid))]
    (fn [destination-id]
      (or (contains? top-row    destination-id)
          (contains? bottom-row destination-id)
          (contains? left-row   destination-id)
          (contains? right-row  destination-id)))))

(defn count-occurrences [grid]
  (let [flat-grid (flatten grid)]
    (fn [destination-id]
      (->> flat-grid
           (filter #{destination-id})
           (count)))))

(defn grid-of [dim element]
  (->> element
       (repeat)
       (take dim)
       (repeat)
       (take dim)))

(defn map-rows [f grid]
  (map #(map f %) grid))

(defn part1 [input-file]
  (let [destination-coords (input-file-coords input-file)
        destinations (zipmap (iterate inc 0) destination-coords)
        grid-dim (->> destination-coords (flatten) (apply max) (inc))
        grid (->> (grid-of grid-dim {:distance Float/POSITIVE_INFINITY
                                     :destination-ids []})
                  (populate-grid-with-closest-destination destinations)
                  (map-rows :destination-ids)
                  (map-rows #(if (= 1 (count %))
                               (first %)
                               \.)))
        destination-ids (keys destinations)]
    (->> destination-ids
         (remove (edge? grid))
         (map (count-occurrences grid))
         (apply max))))

;; --- Part Two ---

;; On the other hand, if the coordinates are safe, maybe the best you
;; can do is try to find a region near as many coordinates as
;; possible.

;; For example, suppose you want the sum of the Manhattan distance to
;; all of the coordinates to be less than 32. For each location, add
;; up the distances to all of the given coordinates; if the total of
;; those distances is less than 32, that location is within the
;; desired region. Using the same coordinates as above, the resulting
;; region looks like this:

;; ..........
;; .A........
;; ..........
;; ...###..C.
;; ..#D###...
;; ..###E#...
;; .B.###....
;; ..........
;; ..........
;; ........F.

;; In particular, consider the highlighted location 4,3 located at the
;; top middle of the region. Its calculation is as follows, where
;; abs() is the absolute value function:

;; Distance to coordinate A: abs(4-1) + abs(3-1) =  5
;; Distance to coordinate B: abs(4-1) + abs(3-6) =  6
;; Distance to coordinate C: abs(4-8) + abs(3-3) =  4
;; Distance to coordinate D: abs(4-3) + abs(3-4) =  2
;; Distance to coordinate E: abs(4-5) + abs(3-5) =  3
;; Distance to coordinate F: abs(4-8) + abs(3-9) = 10
;; Total distance: 5 + 6 + 4 + 2 + 3 + 10 = 30

;; Because the total distance to all coordinates (30) is less than 32,
;; the location is within the region.

;; This region, which also includes coordinates D and E, has a total
;; size of 16.

;; Your actual region will need to be much larger than this example,
;; though, instead including all locations with a total distance of
;; less than 10000.

;; What is the size of the region containing all locations which have
;; a total distance to all given coordinates of less than 10000?

(defn distances-to [destinations coord]
  {:coord coord
   :destination-distances (map (fn [[destination-id destination-coord]]
                                 {:destination-id destination-id
                                  :distance (manhattan-distance coord destination-coord)})
                               destinations)})

(defn part2 [input-file]
  (let [max-closeness (cond (= "input" input-file) 10000
                            (= "input-test" input-file) 32
                            :else nil)
        destination-coords (input-file-coords input-file)
        destinations (zipmap (iterate inc 0) destination-coords)
        grid-dim (->> destination-coords (flatten) (apply max) (inc))
        grid (for [x (range grid-dim)
                   y (range grid-dim)]
               [x y])]
    (->> grid
         (map #(distances-to destinations %))
         (map (fn [{coord :coord destination-distances :destination-distances}]
                {:coord coord
                 :closeness (->> destination-distances
                                 (map :distance)
                                 (reduce +))}))
         (filter #(> max-closeness (:closeness %)))
         (count))))
