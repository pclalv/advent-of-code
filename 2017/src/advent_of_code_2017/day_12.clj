(ns advent-of-code-2017.day-12)
(require 'clojure.set)

;; --- Day 12: Digital Plumber ---

;; Walking along the memory banks of the stream, you find a small
;; village that is experiencing a little confusion: some programs
;; can't communicate with each other.

;; Programs in this village communicate using a fixed system of
;; pipes. Messages are passed between programs using these pipes, but
;; most programs aren't connected to each other directly. Instead,
;; programs pass messages between each other until the message reaches
;; the intended recipient.

;; For some reason, though, some of these messages aren't ever
;; reaching their intended recipient, and the programs suspect that
;; some pipes are missing. They would like you to investigate.

;; You walk through the village and record the ID of each program and
;; the IDs with which it can communicate directly (your puzzle
;; input). Each program has one or more programs with which it can
;; communicate, and these pipes are bidirectional; if 8 says it can
;; communicate with 11, then 11 will say it can communicate with 8.

;; You need to figure out how many programs are in the group that
;; contains program ID 0.

;; For example, suppose you go door-to-door like a travelling salesman
;; and record the following list:

;; 0 <-> 2
;; 1 <-> 1
;; 2 <-> 0, 3, 4
;; 3 <-> 2, 4
;; 4 <-> 2, 3, 6
;; 5 <-> 6
;; 6 <-> 4, 5

;; In this example, the following programs are in the group that
;; contains program ID 0:

;; - Program 0 by definition.
;; - Program 2, directly connected to program 0.
;; - Program 3 via program 2.
;; - Program 4 via program 2.
;; - Program 5 via programs 6, then 4, then 2.
;; - Program 6 via programs 4, then 2.

;; Therefore, a total of 6 programs are in this group; all but program
;; 1, which has a pipe that connects it to itself.

;; How many programs are in the group that contains program ID 0?

(defn read-program-pipe-file [filename]
  (let [raw-contents (->> filename
                          (str "resources/day-12/")
                          slurp
                          clojure.string/trim-newline)
        lines (clojure.string/split raw-contents #"\n")]
    (->> lines
         (map #(clojure.string/split % #" \<\-\> "))
         (map (fn [[prog pipes]]
                [(Integer/parseInt prog) (map #(Integer/parseInt %)
                                              (clojure.string/split pipes #", "))]))
         (into {}))))

(defn connections-to-program [program-pipes program]
  (let [all-programs (->> program-pipes
                          keys
                          set)]
    (loop [programs-that-can-talk-to-current-programs #{program}
           current-programs #{program}]
      (let [current-program-pipes (->> current-programs
                                       (mapcat #(get program-pipes %))
                                       (into #{}))
            current-program-pipes' (clojure.set/difference current-program-pipes programs-that-can-talk-to-current-programs)
            programs-that-can-talk-to-current-programs' (clojure.set/union programs-that-can-talk-to-current-programs
                                                                           current-program-pipes')
            programs-that-cant-talk-to-current-programs' (clojure.set/difference all-programs
                                                                                 programs-that-can-talk-to-current-programs')
            possibly-missed-programs (doall (->> programs-that-cant-talk-to-current-programs'
                                                 (filter (fn [program]
                                                           (some (set (get program-pipes program))
                                                                 current-programs)))))
            current-program-pipes'' (concat current-program-pipes'
                                            possibly-missed-programs)]
        (if (empty? current-program-pipes'')
          (doall (sort programs-that-can-talk-to-current-programs))
          (recur programs-that-can-talk-to-current-programs'
                 current-program-pipes''))))))


(defn connections-to-zero [program-file]
  (let [program-pipes (read-program-pipe-file program-file)]
    (connections-to-program program-pipes 0)))

(defn connections-to-programs [program-pipes programs]
  (reduce (fn [connections program]
            (if (contains? (set (flatten (vals connections))) program)
              connections
              (assoc connections
                     program
                     (connections-to-program program-pipes program))))
          {}
          programs))

(defn groups [program-file]
  (let [program-pipes (read-program-pipe-file program-file)
        all-programs (->> program-pipes
                          keys
                          (into #{}))]
    (loop [programs #{0}
           all-program-talk-groups {}]
      (let [program-talk-groups (connections-to-programs program-pipes programs)
            all-program-talk-groups' (merge all-program-talk-groups
                                            program-talk-groups)
            included-programs (->> all-program-talk-groups'
                                   vals
                                   flatten
                                   (into #{}))
            isolated-programs (clojure.set/difference all-programs
                                                      included-programs)]
        (if (empty? isolated-programs)
          all-program-talk-groups'
          (recur isolated-programs
                 all-program-talk-groups'))))))

;; start with programs that can talk to zero
;; then take all of the programs that can't talk to zero and one by one compute their groups
