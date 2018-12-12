(ns advent-of-code-2018.day-04)

;; --- Day 4: Repose Record ---

;; You've sneaked into another supply closet - this time, it's across
;; from the prototype suit manufacturing lab. You need to sneak inside
;; and fix the issues with the suit, but there's a guard stationed
;; outside the lab, so this is as close as you can safely get.

;; As you search the closet for anything that might help, you discover
;; that you're not the first person to want to sneak in. Covering the
;; walls, someone has spent an hour starting every midnight for the
;; past few months secretly observing this guard post! They've been
;; writing down the ID of the one guard on duty that night - the Elves
;; seem to have decided that one guard was enough for the overnight
;; shift - as well as when they fall asleep or wake up while at their
;; post (your puzzle input).

;; For example, consider the following records, which have already
;; been organized into chronological order:

;; [1518-11-01 00:00] Guard #10 begins shift
;; [1518-11-01 00:05] falls asleep
;; [1518-11-01 00:25] wakes up
;; [1518-11-01 00:30] falls asleep
;; [1518-11-01 00:55] wakes up
;; [1518-11-01 23:58] Guard #99 begins shift
;; [1518-11-02 00:40] falls asleep
;; [1518-11-02 00:50] wakes up
;; [1518-11-03 00:05] Guard #10 begins shift
;; [1518-11-03 00:24] falls asleep
;; [1518-11-03 00:29] wakes up
;; [1518-11-04 00:02] Guard #99 begins shift
;; [1518-11-04 00:36] falls asleep
;; [1518-11-04 00:46] wakes up
;; [1518-11-05 00:03] Guard #99 begins shift
;; [1518-11-05 00:45] falls asleep
;; [1518-11-05 00:55] wakes up

;; Timestamps are written using year-month-day hour:minute format. The
;; guard falling asleep or waking up is always the one whose shift
;; most recently started. Because all asleep/awake times are during
;; the midnight hour (00:00 - 00:59), only the minute portion (00 -
;; 59) is relevant for those events.

;; Visually, these records show that the guards are asleep at these
;; times:

;; Date   ID   Minute
;;             000000000011111111112222222222333333333344444444445555555555
;;             012345678901234567890123456789012345678901234567890123456789
;; 11-01  #10  .....####################.....#########################.....
;; 11-02  #99  ........................................##########..........
;; 11-03  #10  ........................#####...............................
;; 11-04  #99  ....................................##########..............
;; 11-05  #99  .............................................##########.....

;; The columns are Date, which shows the month-day portion of the
;; relevant day; ID, which shows the guard on duty that day; and
;; Minute, which shows the minutes during which the guard was asleep
;; within the midnight hour. (The Minute column's header shows the
;; minute's ten's digit in the first row and the one's digit in the
;; second row.) Awake is shown as ., and asleep is shown as #.

;; Note that guards count as asleep on the minute they fall asleep,
;; and they count as awake on the minute they wake up. For example,
;; because Guard #10 wakes up at 00:25 on 1518-11-01, minute 25 is
;; marked as awake.

;; If you can figure out the guard most likely to be asleep at a
;; specific time, you might be able to trick that guard into working
;; tonight so you can have the best chance of sneaking in. You have
;; two strategies for choosing the best guard/minute combination.

;; Strategy 1: Find the guard that has the most minutes asleep. What
;; minute does that guard spend asleep the most?

;; In the example above, Guard #10 spent the most minutes asleep, a
;; total of 50 minutes (20+25+5), while Guard #99 only slept for a
;; total of 30 minutes (10+10+10). Guard #10 was asleep most during
;; minute 24 (on two days, whereas any other minute the guard was
;; asleep was only seen on one day).

;; While this example listed the entries in chronological order, your
;; entries are in the order you found them. You'll need to organize
;; them before they can be analyzed.

;; What is the ID of the guard you chose multiplied by the minute you
;; chose? (In the above example, the answer would be 10 * 24 = 240.)

(def day "04")

(defn input-file-lines [input-file]
  (-> (str "resources/day_" day "/" input-file)
      (slurp)
      (clojure.string/split #"\n")))

(defn parse-input-file [input-file]
  (->> input-file
       (input-file-lines)
       (map #(->> %
                  (re-find #"\[(\d{4}\-\d{2}\-\d{2}) (\d{2}:\d{2})\] (Guard #(\d+) )??(\w+ \w+)$")
                  (drop 1)
                  (zipmap [:date :time :_ :guard-id :action])))
       (map #(dissoc % :_))))

(defn normalize [entries]
  (loop [normalized-entries []
         [entry1 entry2 & remaining-entries] (into '() entries)]
    (if (nil? entry2)
      (conj normalized-entries entry1)
      (let [normalized-entry2 (if (:guard-id entry2)
                                entry2
                                (->> (get entry1 :guard-id)
                                     (assoc entry2 :guard-id)))]
        (recur (conj normalized-entries entry1)
               (conj remaining-entries normalized-entry2))))))

(defn sleep-period [{start-time :time :as falls-asleep}
                    {end-time :time :as wakes-up}]
  (let [[start-minute end-minute] (->> [start-time end-time]
                                       (map #(-> %
                                                 (clojure.string/split #":")
                                                 (last)
                                                 (Integer/parseInt))))
        duration-minutes (- end-minute start-minute)]
    {:duration-minutes duration-minutes
     :start-minute start-minute
     :end-minute end-minute}))

(defn minute-slept-tally [guard]
  (->> guard
       (:sleep-periods)
       (reduce (fn [minutes-slept-tally {start-minute :start-minute
                                         end-minute :end-minute
                                         :as sleep-period}]
                 (let [minutes-slept (flatten (list (repeat start-minute 0)
                                                    (repeat (- end-minute start-minute) 1)
                                                    (repeat (- 59 end-minute) 0)))]
                   (map + minutes-slept-tally minutes-slept)))
               (vec (repeat 59 0)))))

(defn part1 [input-file]
  (let [entries (->> input-file
                     (parse-input-file)
                     (sort-by (juxt :date :time))
                     (reverse)
                     (normalize))
        guards (->> entries
                    (group-by :guard-id)
                    (map (fn [[guard-id entries]]
                           (let [sleep-periods (->> entries
                                                    (remove #(= "begins shift" (:action %)))
                                                    (partition 2)
                                                    (map (partial apply sleep-period)))
                                 total-sleep-time (->> sleep-periods
                                                       (map :duration-minutes)
                                                       (reduce +))]
                             {:guard-id guard-id
                              :sleep-periods sleep-periods
                              :total-sleep-time total-sleep-time}))))
        sleepiest-guard (apply max-key :total-sleep-time guards)
        minute-slept-most (->> sleepiest-guard
                               (minute-slept-tally)
                               (map-indexed vector)
                               (apply max-key second)
                               (first))]
    (* minute-slept-most
       (Integer/parseInt (:guard-id sleepiest-guard)))))

;; --- Part Two ---

;; Strategy 2: Of all guards, which guard is most frequently asleep on
;; the same minute?

;; In the example above, Guard #99 spent minute 45 asleep more than
;; any other guard or minute - three times in total. (In all other
;; cases, any guard spent any minute asleep at most twice.)

;; What is the ID of the guard you chose multiplied by the minute you
;; chose? (In the above example, the answer would be 99 * 45 = 4455.)

(defn part2 [input-file]
  (let [entries (->> input-file
                     (parse-input-file)
                     (sort-by (juxt :date :time))
                     (reverse)
                     (normalize))
        guards (->> entries
                    (group-by :guard-id)
                    (map (fn [[guard-id entries]]
                           (let [sleep-periods (->> entries
                                                    (remove #(= "begins shift" (:action %)))
                                                    (partition 2)
                                                    (map (partial apply sleep-period)))
                                 total-sleep-time (->> sleep-periods
                                                       (map :duration-minutes)
                                                       (reduce +))]
                             {:guard-id guard-id
                              :sleep-periods sleep-periods
                              :total-sleep-time total-sleep-time}))))
        guards-minute-slept-most (->> guards
                                      (map (fn [guard]
                                             (->> guard
                                                  (minute-slept-tally)
                                                  (map-indexed vector)
                                                  (apply max-key second)
                                                  (assoc guard :minute-slept-most)))))
        guard-most-frequently-alseep-at-any-minute(->> guards-minute-slept-most
                                                       (map (fn [{guard-id :guard-id minute-slept-most :minute-slept-most}]
                                                              {:guard-id guard-id :minute-slept-most  minute-slept-most}))
                                                       (sort-by (fn [{[minute times-slept] :minute-slept-most}]
                                                                  times-slept))
                                                       (last)
                                                       )]
    (* (->> guard-most-frequently-alseep-at-any-minute
            (:guard-id)
            (Integer/parseInt))
       (->> guard-most-frequently-alseep-at-any-minute
            (:minute-slept-most)
            (first)))))
