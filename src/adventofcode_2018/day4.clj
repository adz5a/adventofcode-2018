(ns adventofcode-2018.day4
  (:require [clojure.string :refer [split]]
            [clojure.spec.alpha :as s]
            [clojure.test :as t]
            [clojure.pprint :as pp :refer [pprint]]))

(def test-data-string
"[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-01 00:30] falls asleep
[1518-11-01 00:55] wakes up
[1518-11-01 23:58] Guard #99 begins shift
[1518-11-02 00:40] falls asleep
[1518-11-02 00:50] wakes up
[1518-11-03 00:05] Guard #10 begins shift
[1518-11-03 00:24] falls asleep
[1518-11-03 00:29] wakes up
[1518-11-04 00:02] Guard #99 begins shift
[1518-11-04 00:36] falls asleep
[1518-11-04 00:46] wakes up
[1518-11-05 00:03] Guard #99 begins shift
[1518-11-05 00:45] falls asleep
[1518-11-05 00:55] wakes up")

(def lines (split test-data-string #"\n"))

(comment (pprint lines))

(def action->regex {:shift-start #"\[(\d+)-0?(\d+)-0?(\d+) 0?(\d+):0?(\d+)\] Guard #(\d+) begins shift"
                    :start-sleeping #"\[(\d+)-0?(\d+)-0?(\d+) 0?(\d+):0?(\d+)\] falls asleep"
                    :wakes-up #"\[(\d+)-0?(\d+)-0?(\d+) 0?(\d+):0?(\d+)\] wakes up"})

(defn parse-line [line]
  (reduce #(let [[action re] %2
                 matches (re-matches re line)]
             (when matches
               (reduced (->> matches
                             (drop 1)
                             (map read-string)
                             (vector action)))))
          nil
          action->regex))

(def parsed-lines (map parse-line lines))

(s/def ::log-date
  (s/cat :year number?
         :month number?
         :day number?
         :hour number?
         :minute number?))

(s/def ::log-entry
  (s/or :shift-start (s/cat :type (partial = :shift-start)
                            :details (s/spec (s/cat :date ::log-date
                                                    :id number?)))
        :start-sleeping (s/cat :type (partial = :start-sleeping)
                            :details (s/spec (s/cat :date ::log-date)))
        :wakes-up (s/cat :type (partial = :wakes-up)
                            :details (s/spec (s/cat :date ::log-date)))))

(comment (pprint parsed-lines))

(def data (map (comp
                 second
                 (partial s/conform ::log-entry)) parsed-lines))

(defn is-type [t log-entry]
  (= t (:type log-entry)))

;; We want to be able to define a shift with all its sleeps cycle
(s/def ::shift
  (s/cat :guard (partial is-type :shift-start)
         :sleep-cycles (s/+ (s/cat :sleeps (partial is-type :start-sleeping)
                                   :wakes-up (partial is-type :wakes-up)))))
(s/def ::timeline
  (s/+ ::shift))

(def timeline (s/conform ::timeline data))

(comment (pprint timeline))

(defn cycle-start [sleep-cycle]
  (get-in sleep-cycle [:sleeps :details :date :minute]))
(defn cycle-end [sleep-cycle]
  (get-in sleep-cycle [:wakes-up :details :date :minute]))
(defn guard-id [shift]
  (get-in shift [:guard :details :id]))

(defn sleep-stats [shift]
  (let [id (guard-id shift)
        minutes (apply hash-map
                       (interleave (range 60)
                                   (range 0 1 0)))
        sleep-cycles-ranges (map #(range
                                    (cycle-start %)
                                    (cycle-end %))
                                 (:sleep-cycles shift))
        minutes->sleep-time (reduce
                              (fn [minutes sleep-range]
                                (reduce (fn [minutes minute]
                                          (update minutes minute inc))
                                        minutes
                                        sleep-range))
                              minutes
                              sleep-cycles-ranges)]
    {:id id
     :sleep-map minutes->sleep-time}))

(defn get-most-slept-minute [minutes]
  (first (sort-by val > minutes)))

(let [id->minutes-slept (into {}
                              (map (fn [[k v]]
                                     [k (apply merge-with + (map :sleep-map v))]))
                              (group-by :id
                                        (map sleep-stats
                                             timeline)))]
  (sort-by
    #(get-in % [1 :sleep-length])
    >
    (map (fn [[id minutes]]
           [id {:most-slept-minute (get-most-slept-minute minutes)
                :sleep-length (reduce + (map val minutes))}])
         id->minutes-slept)))

