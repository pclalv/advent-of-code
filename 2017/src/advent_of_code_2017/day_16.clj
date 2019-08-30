(ns advent-of-code-2017.day-16
  (:require [clojure.string :as str]
            [clojure.math.numeric-tower :as math]))

;; --- Day 16: Permutation Promenade ---

;; You come upon a very unusual sight; a group of programs here appear
;; to be dancing.

;; There are sixteen programs in total, named a through p. They start
;; by standing in a line: a stands in position 0, b stands in position
;; 1, and so on until p, which stands in position 15.

;; The programs' dance consists of a sequence of dance moves:

;; Spin, written sX, makes X programs move from the end to the front, but maintain their order otherwise. (For example, s3 on abcde produces cdeab).
;; Exchange, written xA/B, makes the programs at positions A and B swap places.
;; Partner, written pA/B, makes the programs named A and B swap places.
;; For example, with only five programs standing in a line (abcde), they could do the following dance:

;; s1, a spin of size 1: eabcd.
;; x3/4, swapping the last two programs: eabdc.
;; pe/b, swapping programs e and b: baedc.
;; After finishing their dance, the programs end up in order baedc.

;; You watch the dance for a while and record their dance moves (your
;; puzzle input). In what order are the programs standing after their
;; dance?

(defn char-range [start end]
  (map char (range (int start) (inc (int end)))))

(defn spin? [s]
  (str/starts-with? s "s"))

(defn exchange? [s]
  (str/starts-with? s "x"))

(defn partner? [s]
  (str/starts-with? s "p"))

(defn dance-move-spin [{:keys [distance]}
                       {:keys [progs-vec progs-map]}]
  (let [len (count progs-vec)
        progs-vec' (->> progs-vec
                        (cycle)
                        (drop (math/abs (- len distance)))
                        (take len)
                        (vec))
        progs-map' (zipmap progs-vec' (range len))]
    {:progs-vec progs-vec'
     :progs-map progs-map'}))

(defn dance-move-exchange [{:keys [idx1 idx2]}
                           {:keys [progs-vec progs-map]}]
  (let [prog-a (nth progs-vec idx1)
        prog-b (nth progs-vec idx2)
        progs-vec' (-> progs-vec
                       (assoc idx1 prog-b)
                       (assoc idx2 prog-a))
        progs-map' (zipmap progs-vec' (range (count progs-vec')))]
    {:progs-vec progs-vec'
     :progs-map progs-map'}))

(defn dance-move-partner [{:keys [prog-a prog-b]}
                          {:keys [progs-vec progs-map]}]
  (let [idx1 (progs-map prog-a)
        idx2 (progs-map prog-b)
        progs-map' (-> progs-map
                       (assoc prog-a idx2)
                       (assoc prog-b idx1))
        progs-vec' (-> progs-vec
                       (assoc idx1 prog-b)
                       (assoc idx2 prog-a))]
    {:progs-vec progs-vec'
     :progs-map progs-map'}))

(defn parse-dance-move-spin [s]
  (let [[_ distance] (re-matches #"s(\d+)" s)]
    (partial dance-move-spin
             {:distance (Integer/parseInt distance)})))

(defn parse-dance-move-exchange [s]
  (let [[_ idx1 idx2] (re-matches #"x(\d+)/(\d+)" s)]
    (partial dance-move-exchange
             {:idx1 (Integer/parseInt idx1)
              :idx2 (Integer/parseInt idx2)})))

(defn parse-dance-move-partner [s]
  (let [[_ prog-a prog-b] (re-matches #"p(\w)/(\w)" s)]
    (partial dance-move-partner
             {:prog-a (first (char-array prog-a))
              :prog-b (first (char-array prog-b))})))

(defn parse-dance-move [s]
  (cond (spin? s) (parse-dance-move-spin s)
        (exchange? s) (parse-dance-move-exchange s)
        (partner? s) (parse-dance-move-partner s)
        :else (throw (Exception. (str "bad dance move: " s)))))

(def programs (let [progs (vec (char-range \a \p))]
                {:progs-vec progs
                 :progs-map (zipmap progs (range (count progs)))}))
(def input (map parse-dance-move (-> "resources/day-16/input"
                                     slurp
                                     clojure.string/trim-newline
                                     (str/split #","))))


(def test-programs (let [progs (vec (char-range \a \e))]
                     {:progs-vec progs
                      :progs-map (zipmap progs (range (count progs)))}))
(def test-input (map parse-dance-move ["s1" "x3/4" "pe/b"]))

(comment
  ;; test data
  (->> test-input
       (reduce (fn [progs dance-move]
                 (dance-move progs))
               test-programs)
       (:progs-vec)
       (apply str))

  ;; real deal
  (->> input
       (reduce (fn [progs dance-move]
                 (dance-move progs))
               programs)
       (:progs-vec)
       (apply str)))

;; --- Part Two ---

;; Now that you're starting to get a feel for the dance moves, you
;; turn your attention to the dance as a whole.

;; Keeping the positions they ended up in from their previous dance,
;; the programs perform it again and again: including the first dance,
;; a total of one billion (1000000000) times.

;; In the example above, their second dance would begin with the order
;; baedc, and use the same dance moves:

;; s1, a spin of size 1: cbaed.
;; x3/4, swapping the last two programs: cbade.
;; pe/b, swapping programs e and b: ceadb.
;; In what order are the programs standing after their billion dances?

