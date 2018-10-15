(ns advent-of-code-2017.day-11)

;; --- Day 11: Hex Ed ---

;; Crossing the bridge, you've barely reached the other side of the
;; stream when a program comes up to you, clearly in distress. "It's
;; my child process," she says, "he's gotten lost in an infinite
;; grid!"

;; Fortunately for her, you have plenty of experience with infinite grids.

;; Unfortunately for you, it's a hex grid.

;; The hexagons ("hexes") in this grid are aligned such that adjacent
;; hexes can be found to the north, northeast, southeast, south,
;; southwest, and northwest:

;;     \ n  /
;;  nw +--+ ne
;;    /    \
;;  -+      +-
;;    \    /
;; sw +--+ se
;;   / s  \

;; You have the path the child process took. Starting where he
;; started, you need to determine the fewest number of steps required
;; to reach him. (A "step" means to move from the hex you are in to
;; any adjacent hex.)

;; For example:

;; ne,ne,ne is 3 steps away.
;; ne,ne,sw,sw is 0 steps away (back where you started).
;; ne,ne,s,s is 2 steps away (se,se).
;; se,sw,se,sw,sw is 3 steps away (s,s,sw).

(defn cancel-out? [[step1 step2]]
  (or (and (= :ne step1) (= :sw step2))
      (and (= :sw step1) (= :ne step2))
      (and (= :nw step1) (= :se step2))
      (and (= :se step1) (= :nw step2))
      (and (= :n step1) (= :s step2))
      (and (= :s step1) (= :n step2))))

(defn simplify [[step1 step2]]
  (cond
    (or (and (= :ne step1) (= :s step2))
        (and (= :s step1) (= :ne step2))) :se
    (or (and (= :se step1) (= :n step2))
        (and (= :n step1) (= :se step2))) :ne
    (or (and (= :nw step1) (= :s step2))
        (and (= :s step1) (= :nw step2))) :sw
    (or (and (= :sw step1) (= :n step2))
        (and (= :n step1) (= :sw step2))) :nw
    (cancel-out? [step1 step2]) nil
    :else [step1 step2]))

(defn remove-cancelling-steps [groupings]
  (let [reduced-groupings (map simplify groupings)]
    (->> reduced-groupings
         flatten
         (remove nil?))))

(defn reduce-adjacent-steps
  ([steps]
   (reduce-adjacent-steps steps steps))
  ([steps previous-steps]
   (let [even-groupings (partition 2 2 [nil] steps)
         partially-reduced-steps (remove-cancelling-steps even-groupings)]
     (if (empty? partially-reduced-steps)
       partially-reduced-steps
       (let [[head & tail] partially-reduced-steps
             odd-groupings (partition 2 2 [nil] tail)
             fully-reduced-steps (as-> odd-groupings gs
                                   (remove-cancelling-steps gs)
                                   (conj gs head))]
         (if (or (empty? fully-reduced-steps) (= fully-reduced-steps previous-steps))
           fully-reduced-steps
           (recur fully-reduced-steps steps)))))))

(defn cancel [direction opposite step-freqs]
  (let [dir-count (direction step-freqs)
        opp-count (opposite step-freqs)]
    (if (some nil? [dir-count opp-count])
      step-freqs
      (let [diff (- dir-count opp-count)
            step-freqs' (dissoc step-freqs direction opposite)]
        (cond (pos? diff) (assoc step-freqs' direction diff)
              (neg? diff) (assoc step-freqs' opposite diff)
              :else step-freqs')))))

(defn sum [dir1 dir2 sum-dir step-freqs]
  (let [dir1-count (dir1 step-freqs)
        dir2-count (dir2 step-freqs)]
    (if (some nil? [dir1-count dir2-count])
      step-freqs
      (let [[greater-dir lesser-dir] (if (> dir1-count dir2-count)
                                       [dir1 dir2]
                                       [dir2 dir1])
            step-freqs' (dissoc step-freqs dir1 dir2 sum-dir)]
        (-> step-freqs'
            (dissoc lesser-dir)
            (assoc greater-dir (- (greater-dir step-freqs)
                                  (lesser-dir step-freqs)))
            (assoc sum-dir (lesser-dir step-freqs)))))))

(defn neg-to-pos [dir1 dir2 freq-map]
  (let [dir1-count (get freq-map dir1 0)
        dir2-count (get freq-map dir2 0)
        freq-map' (dissoc freq-map dir1 dir2)]
    (cond (neg? dir1-count) (assoc freq-map' dir2 (- dir2-count dir1-count))
          (neg? dir2-count) (assoc freq-map' dir1 (- dir1-count dir2-count))
          :else freq-map)))

(defn freq-map-to-coll [freq-map]
  (prn "freq-map" freq-map)
  (mapcat (fn [[x n]] (repeat n x)) freq-map))

(defn reduce-steps [steps]
  (let [reduced-steps (->> steps
                           frequencies
                           (into {})

                           (cancel :n :s)
                           (cancel :ne :sw)
                           (cancel :nw :se)

                           ;; (sum :n :se :ne)
                           ;; (sum :n :sw :nw)

                           ;; (sum :ne :nw :n)
                           ;; (sum :ne :s :se)

                           ;; ;(sum :se :n :ne) ; dup
                           ;; (sum :se :sw :s)

                           ;; ;(sum :s :ne :se) ; dup
                           ;; (sum :s :nw :sw)

                           ;; ;(sum :sw :se :s) ; dup
                           ;; ;(sum :sw :n :nw) ; dup

                           ;; ;(sum :nw :s :sw) ; dup
                           ;; ;(sum :nw :ne :n) ; dup

                           ;; (neg-to-pos :n :s)
                           (neg-to-pos :ne :sw)
                           ;; (neg-to-pos :se :nw)
                           (freq-map-to-coll)
                           (into []))]
    (if (= reduced-steps steps)
      reduced-steps
      (reduce-steps reduced-steps))))

(def axial-coordinates
  {:n  [0 -1]
   :ne [1 -1]
   :se [1  0]
   :s  [0  1]
   :sw [-1 1]
   :nw [-1 0]})

(defn axial-sum [[q0 r0] [q1 r1]]
  [(+ q0 q1)
   (+ r0 r1)])

(defn abs [n] (max n (- n)))

(defn axial-magnitude [qr]
  (->> qr
       (map abs)
       (reduce +)))

(defn track-steps []
  (let [steps-strings (-> "resources/day-11/input"
                          slurp
                          (clojure.string/trim-newline)
                          (clojure.string/split #","))]
    (->> steps-strings
         (map keyword)
         (map axial-coordinates)
         (reduce (fn [{current :current max :max} qr]
                   (let [new-current (axial-sum current qr)]
                     {:current new-current
                      :max (if (> (axial-magnitude current) (axial-magnitude max))
                             current
                             max)}))
                 {:current [0 0]
                  :max [0 0]})
         (map (fn [[k value]]
                [k (->> value
                        (map abs)
                        (into []))])))))

(def part1 (:current (track-steps)))
(def part2 (:max (track-steps)))
