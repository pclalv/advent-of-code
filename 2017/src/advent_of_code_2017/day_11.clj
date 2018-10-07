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
  (let [reduced-groupings (remove cancel-out? groupings)]
    (->> reduced-groupings
         flatten
         (remove nil?))))

(defn reduce-steps
  ([steps]
   (reduce-steps steps steps))
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
