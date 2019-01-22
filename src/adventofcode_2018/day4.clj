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


(def action->regex {:shift-start #"\[(\d+)-(\d+)-(\d+) (\d+):(\d+)\] Guard #(\d+) begins shift"
                    :start-sleeping #"\[(\d+)-(\d+)-(\d+) (\d+):(\d+)\] falls asleep"
                    :wakes-up #"\[(\d+)-(\d+)-(\d+) (\d+):(\d+)\] wakes up"})

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

(def data (map (comp
                 second
                 (partial s/conform ::log-entry)) parsed-lines))

(defn is-type [t entry]
  (= t (:type entry)))

;; We want to be able to define a shift with all its sleeps cycle
(s/def ::shift
  (s/cat :start-shift (partial is-type :shift-start)
         :sleep-cycles (s/+ (s/cat :sleeps (partial is-type :start-sleeping)
                                   :wakes-up (partial is-type :wakes-up)))))
(s/def ::timeline
  (s/+ ::shift))

(def timeline (s/conform ::timeline data))
