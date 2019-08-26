(ns advent-of-code-2017.day-14)
(require '[clojure.pprint :refer (cl-format)])
(require '[advent-of-code-2017.day-10 :refer (hash-string)])

;; --- Day 14: Disk Defragmentation ---

;; Suddenly, a scheduled job activates the system's disk
;; defragmenter. Were the situation different, you might sit and watch
;; it for a while, but today, you just don't have that kind of
;; time. It's soaking up valuable system resources that are needed
;; elsewhere, and so the only option is to help it finish its task as
;; soon as possible.

;; The disk in question consists of a 128x128 grid; each square of the
;; grid is either free or used. On this disk, the state of the grid is
;; tracked by the bits in a sequence of knot hashes.

;; A total of 128 knot hashes are calculated, each corresponding to a
;; single row in the grid; each hash contains 128 bits which
;; correspond to individual grid squares. Each bit of a hash indicates
;; whether that square is free (0) or used (1).

;; The hash inputs are a key string (your puzzle input), a dash, and a
;; number from 0 to 127 corresponding to the row. For example, if your
;; key string were flqrgnkx, then the first row would be given by the
;; bits of the knot hash of flqrgnkx-0, the second row from the bits
;; of the knot hash of flqrgnkx-1, and so on until the last row,
;; flqrgnkx-127.

;; The output of a knot hash is traditionally represented by 32
;; hexadecimal digits; each of these digits correspond to 4 bits, for
;; a total of 4 * 32 = 128 bits. To convert to bits, turn each
;; hexadecimal digit to its equivalent binary value, high-bit first: 0
;; becomes 0000, 1 becomes 0001, e becomes 1110, f becomes 1111, and
;; so on; a hash that begins with a0c2017... in hexadecimal would
;; begin with 10100000110000100000000101110000... in binary.

;; Continuing this process, the first 8 rows and columns for key
;; flqrgnkx appear as follows, using # to denote used squares, and
;; . to denote free ones:

;; ##.#.#..-->
;; .#.#.#.#
;; ....#.#.
;; #.#.##.#
;; .##.#...
;; ##..#..#
;; .#...#..
;; ##.#.##.-->
;; |      |
;; V      V

;; In this example, 8108 squares are used across the entire 128x128
;; grid.

;; Given your actual key string, how many squares are used?

;; Your puzzle input is hfdlxzhv.

(def input "hfdlxzhv")
(def input-test "flqrgnkx")

(defn rows [input]
  (for [ints (range 128)
        :let [int ints]]
    (str input "-" int)))

(defn row-hash-to-binary [row-hash]
  (->> (clojure.string/split row-hash #"")
       (map #(Integer/parseInt % 16))
       (map #(cl-format nil "~4,'0',B" %))
       (apply str)))

(defn memory-grid [string]
  (let [row-strings (rows string)
        row-hashes (map advent-of-code-2017.day-10/hash-string row-strings)]
    (map row-hash-to-binary row-hashes)))

(defn count-occupied [string]
  (->> string
       (memory-grid)
       (map frequencies)
       (map #(get % \1))
       (reduce +)))

;; --- Part Two ---

;; Now, all the defragmenter needs to know is the number of regions. A
;; region is a group of used squares that are all adjacent, not
;; including diagonals. Every used square is in exactly one region:
;; lone used squares form their own isolated regions, while several
;; adjacent squares all count as a single region.

;; In the example above, the following nine regions are visible, each
;; marked with a distinct digit:

;; 11.2.3..-->
;; .1.2.3.4
;; ....5.6.
;; 7.8.55.9
;; .88.5...
;; 88..5..8
;; .8...8..
;; 88.8.88.-->
;; |      |
;; V      V

;; Of particular interest is the region marked 8; while it does not
;; appear contiguous in this small view, all of the squares marked 8
;; are connected when considering the whole 128x128 grid. In total, in
;; this example, 1242 regions are present.

;; How many regions are present given your key string?

;; isn't capable of merging regions that make contact "too late" for the algorithm to detect
(defn count-regions
  ([string]
   (let [grid (doall (->> string
                          memory-grid
                          (map #(clojure.string/replace % #"0" "."))
                          (map #(clojure.string/replace % #"1" "R"))
                          (map #(into [] %))
                          (into [])))
         grid-dim (count (first grid))
         addresses (for [row (range grid-dim)
                         col (range grid-dim)
                         :let [address [row col]]]
                     (vec address))]
     (count-regions grid 0 addresses)))
  ([grid region-id [[row col] & addresses]]
   (let [address [row col]]
     (if (and (nil? row) (nil? col))
       {:grid grid
        :region-count region-id}
       (let [curr (get-in grid address)]
         (if (= curr \.)
           (recur grid region-id addresses)
           (let [right-addr [row (inc col)]
                 right (get-in grid right-addr)
                 curr' (cond (and (int? right) (int? curr)) (min right curr)
                             (int? curr) curr
                             (int? right) right
                             :else region-id)
                 next-region-id (if (not= region-id curr')
                                  region-id
                                  (inc region-id))
                 right' (if (= \. right)
                          \.
                          curr')
                 down-addr [(inc row) col]
                 down (get-in grid down-addr)
                 down' (if (= \. down)
                         \.
                         curr')
                 grid' (assoc-in grid address curr')
                 grid'' (if (> 128 (inc col))
                          (assoc-in grid' right-addr right')
                          grid')
                 grid''' (if (> 128 (inc row))
                           (assoc-in grid'' down-addr down')
                           grid'')
                 ;; _ (when (and (= col 3) (= row 8))
                 ;;     (do (println)
                 ;;         (println)
                 ;;         (println "address curr curr'" address curr curr')
                 ;;         (println "right-addr right'" right-addr right right')
                 ;;         (println "down-addr down'" down-addr down down')
                 ;;         (println)
                 ;;         (println)
                 ;;         (throw (Exception. "bail"))))
                 grid'''' (if (and (not= curr \R) (not= curr' \R) (not= curr curr'))
                            (let [new-curr (min curr curr')
                                  old-curr (max curr curr')]
                              (doall (->> grid'''
                                          (map #(doall (replace {old-curr new-curr} %)))
                                          (into []))))
                            grid''')
                 grid''''' (if (and (> 128 (inc col)) (not= right \R) (not= right' \R) (not= right right'))
                             (let [new-right (min right right')
                                   old-right (max right right')]
                               (doall (->> grid''''
                                           (map #(doall (replace {old-right new-right} %)))
                                           (into []))))
                             grid'''')
                 grid'''''' (if (and (> 128 (inc row)) (not= down \R) (not= down' \R) (not= down down'))
                              (let [new-down (min down down')
                                    old-down (max down down')]
                                (doall (->> grid'''''
                                            (map #(doall (replace {old-down new-down} %)))
                                            (into []))))
                              grid''''')]
             (recur grid'''''' next-region-id addresses))))))))

(comment (let [dim 15
               (map #(take dim %) 
                    (take dim (:grid (count-regions input-test))))])
         ;; this is looking good, but 88 needs to be merged into 4.
         ;; seems to be because the slot below 88 is 88, and then 48. and THEN 4.

         ;; if we're changing the integer value of down or right, then
         ;; we need to go back and replace down and right, just like
         ;; we do with curr and curr'.
         (( 0  0 \.  1 \.  2 \. \.  3  3  3  3 \.  4  4)
          (\.  0 \.  1 \.  2 \.  3  3  3  3 \.  4 \.  4)
          (\. \. \. \. 38 \. 39 \.  3  3 \.  4  4  4  4)
          (47 \.  4 \. 38 38 \. 49 \. \.  4  4  4  4 \.)
          (\.  4  4 \. 38 \. \. \. \. \.  4 \.  4  4  4)
          ( 4  4 \. \. 38 \. \.  4  4  4  4  4 \.  4 \.)
          (\.  4 \. \. \.  4 \. \.  4  4 \. \. \. \. \.)
          ( 4  4 \. 88 \.  4  4 \. \.  4  4  4 \. \. 89)
          ( 4  4  4  4 \.  4  4  4  4  4  4 \. \. 89 89)
          ( 4  4 \.  4 \.  4  4  4  4 \. \. \. \. \. \.)
          (\. \. \.  4 \. \. \. \.  4 \. \R \R \. \. \R)
          (\.  4  4  4  4  4  4  4  4  4  4  4 \. \. \.)
          (\. \. \.  4 \.  4  4 \. \. \.  4 \. \. \R \R)
          (\. \R \. \.  4  4 \. \.  4  4  4 \. \. \R \R)
          (\R \. \. \.  4 \. \. \. \.  4  4 \. \R \R \.)))

(comment
  ;; part2 solution
  (->> input
       (count-regions)
       (:grid)
       (flatten)
       (filter int?)
       (into #{})
       (count)))
