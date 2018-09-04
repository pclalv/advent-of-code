(ns advent-of-code-2017.day-7)
(require 'clojure.set)

;; --- Day 7: Recursive Circus ---
;; Wandering further through the circuits of the computer, you come
;; upon a tower of programs that have gotten themselves into a bit of
;; trouble. A recursive algorithm has gotten out of hand, and now
;; they're balanced precariously in a large tower.

;; One program at the bottom supports the entire tower. It's holding a
;; large disc, and on the disc are balanced several more
;; sub-towers. At the bottom of these sub-towers, standing on the
;; bottom disc, are other programs, each holding their own disc, and
;; so on. At the very tops of these sub-sub-sub-...-towers, many
;; programs stand simply keeping the disc below them balanced but with
;; no disc of their own.

;; You offer to help, but first you need to understand the structure
;; of these towers. You ask each program to yell out their name, their
;; weight, and (if they're holding a disc) the names of the programs
;; immediately above them balancing on that disc. You write this
;; information down (your puzzle input). Unfortunately, in their
;; panic, they don't do this in an orderly fashion; by the time you're
;; done, you're not sure which program gave which information.

;; For example, if your list is the following:

;; pbga (66)
;; xhth (57)
;; ebii (61)
;; havc (66)
;; ktlj (57)
;; fwft (72) -> ktlj, cntj, xhth
;; qoyq (66)
;; padx (45) -> pbga, havc, qoyq
;; tknk (41) -> ugml, padx, fwft
;; jptl (61)
;; ugml (68) -> gyxo, ebii, jptl
;; gyxo (61)
;; cntj (57)
;; ...then you would be able to recreate the structure of the towers that looks like this:

;;                      gyxo
;;                  /
;;           ugml - ebii
;;           /      \
;;           |         jptl
;;           |
;;           |         pbga
;;           /        /
;; tknk --- padx - havc
;;           \        \
;;           |         qoyq
;;           |
;;           |         ktlj
;;           \      /
;;           fwft - cntj
;;                  \
;;                     xhth

;; In this example, tknk is at the bottom of the tower (the bottom
;; program), and is holding up ugml, padx, and fwft. Those programs
;; are, in turn, holding up other programs; in this example, none of
;; those programs are holding up any other programs, and are all the
;; tops of their own towers. (The actual tower balancing in front of
;; you is much larger.)

;; Before you're ready to help them, you need to make sure your
;; information is correct. What is the name of the bottom program?

(def program-children-delimiter-re #" -> ")
(def child-delimiter-re #", ")
(def program-name-weight-re #"(\w+)\s\((\d+)\)")
(def path-prefix "resources/day-7/")

(defn parse-program-line [program-line]
  (let [[program-raw children-raw] (-> program-line
                                       (clojure.string/split program-children-delimiter-re))
        [_ program_name weight] (re-find program-name-weight-re program-raw)]
    (if children-raw
      {:program-name program_name
       :weight (Integer/parseInt weight)
       :children (-> children-raw
                     (clojure.string/split child-delimiter-re)
                     (set))}
      {:program-name program_name
       :weight (Integer/parseInt weight)
       :children #{}})))

(defn parse-program-file-contents [program-file-contents]
  (map parse-program-line
       program-file-contents))

(defn read-program-file [file]
  (let [resource-path (str "resources/day-7/" file)]
    (with-open [rdr (clojure.java.io/reader resource-path)]
      (doall (->> rdr
                  (line-seq)
                  (map parse-program-line))))))

(defn find-root-program [filename]
  (let [programs (read-program-file filename)
        parent-programs (filter #(->> %
                                      (:children)
                                      (empty?)
                                      (not))
                                programs)
        child-programs (->> parent-programs
                            (map :children)
                            (apply clojure.set/union))
        root-program (first (filter #(not (contains? child-programs %))
                                    (map :program-name parent-programs)))]
    root-program))

;; --- Part Two ---
;; The programs explain the situation: they can't get down. Rather,
;; they could get down, if they weren't expending all of their energy
;; trying to keep the tower balanced. Apparently, one program has the
;; wrong weight, and until it's fixed, they're stuck here.

;; For any program holding a disc, each program standing on that disc
;; forms a sub-tower. Each of those sub-towers are supposed to be the
;; same weight, or the disc itself isn't balanced. The weight of a
;; tower is the sum of the weights of the programs in that tower.

;; In the example above, this means that for ugml's disc to be
;; balanced, gyxo, ebii, and jptl must all have the same weight, and
;; they do: 61.

;; However, for tknk to be balanced, each of the programs standing on
;; its disc and all programs above it must each match. This means that
;; the following sums must all be the same:

;; ugml + (gyxo + ebii + jptl) = 68 + (61 + 61 + 61) = 251
;; padx + (pbga + havc + qoyq) = 45 + (66 + 66 + 66) = 243
;; fwft + (ktlj + cntj + xhth) = 72 + (57 + 57 + 57) = 243

;; As you can see, tknk's disc is unbalanced: ugml's stack is heavier
;; than the other two. Even though the nodes above ugml are balanced,
;; ugml itself is too heavy: it needs to be 8 units lighter for its
;; stack to weigh 243 and keep the towers balanced. If this change
;; were made, its weight would be 60.

;; Given that exactly one program is the wrong weight, what would its
;; weight need to be to balance the entire tower?

;; okay if i were writing this from scratch, how would i do it?
;; start with the list of programs
;; select programs without children

(defn is-parent-of? [child parent]
  (contains? (:children parent)
             (:program-name child)))

(defn prune-children [children]
  (fn [program]
    (if (not= 0 (:weight-children program))
      (assoc program :children #{})
      program)))

(defn assoc-weight-children [leaves]
  (fn [program]
    (assoc program
           :weight-children
           (->> leaves
                (filter #(= (:program-name program)
                            (:parent %)))
                (map :weight)
                (apply +)))))

(defn is-not-leaf? [leaves]
  (let [leaf-names (->> leaves
                        (map :program-name)
                        (into #{}))]
    (fn [program]
      (not (contains? leaf-names
                      (:program-name program))))))

(defn assoc-total-weight [leaf]
  (assoc leaf
         :total-weight
         (+ (:weight leaf)
            (or (:weight-children leaf)
                0))))

(defn balanced-branch? [branches]
  (->> branches
       (map assoc-total-weight)
       (map :total-weight)
       (apply =)))

(defn balanced-branches? [branches]
  (print "unbalanced branches count: ")
  (prn (->> branches
                (filter #(not (balanced-branch? %)))
                (count)))
  (prn)
  (every? balanced-branch? branches))

(defn find-unlike [[a b c & rst]]
  (if (and (= a b c)
           (not (nil? a)))
    (recur rst)
    (if (= a b)
      c
      (if (= a c)
        b
        a))))

(defn find-unbalanced [branches]
  (let [unbalanced-branches (->> branches
                                 (filter #(not (balanced-branch? %))))
        _ (prn "unbalanced-branches" unbalanced-branches)
        unbalanced-branch (first unbalanced-branches)
        total-weights (->> unbalanced-branch
                           (map assoc-total-weight)
                           (map :total-weight))
        incorrect-total-weight (find-unlike total-weights)
        correct-total-weight (first (filter #(not= % incorrect-total-weight) total-weights))
        correction (- correct-total-weight incorrect-total-weight)
        programs-with-incorrect-total-weight (->> unbalanced-branch
                                                 (map assoc-total-weight)
                                                 (filter #(= incorrect-total-weight
                                                             (:total-weight %))))
        program-with-incorrect-total-weight (first programs-with-incorrect-total-weight)
        _ (prn program-with-incorrect-total-weight)]
    (assoc program-with-incorrect-total-weight
           :correct-weight
           (+ correction
              (:weight program-with-incorrect-total-weight)))))

(defn is-parent-of-leaf? [leaves]
  (let [leaf-parents (->> leaves
                          (map :parent)
                          (into #{}))]
    (fn [program]
      (contains? leaf-parents (:program-name program)))))

(defn inspect-branches [programs]
  (print "(count programs): ")
  (prn (count programs))
  (loop [leaves (->> programs
                     (filter #(empty? (:children %))))]
    (let [leaves-grouped-by-parent (->> leaves
                                        (group-by :parent)
                                        (vals))]
      ;; (print "inspecting: ")
      ;; (prn leaves)
      ;; (prn)
      (if (balanced-branches? leaves-grouped-by-parent)
        ;; the problem is here. i'm trying to build the tree from the
        ;; leaves by tracing back through their parents, but i'm
        ;; liable to miss things because not all parents at every
        ;; level have descendants?
        (let [parents-of-leaves (->> programs
                                     (filter (is-parent-of-leaf? leaves))
                                     (map (assoc-weight-children leaves)))]
          (prn "still balanced")
          (print "(count leaves): ")
          (prn (count leaves))
          (print "(count parents-of-leaves): ")
          (prn (count parents-of-leaves))
          (recur parents-of-leaves))
        (do (prn "unbalanced")
            (find-unbalanced leaves-grouped-by-parent))))))

(defn assoc-parent [programs]
  (map (fn [program]
         (let [parent-name (->> programs
                                (filter (partial is-parent-of? program))
                                (first))
               _ (if (= "krgdzw" (:program-name program))
                   (prn "parent-name" parent-name)
                   nil)]
           (assoc program :parent parent-name)))
       programs))

(defn balance-tower [file]
  (->> file
       (read-program-file)
       (assoc-parent)))
       ;(inspect-branches)))
