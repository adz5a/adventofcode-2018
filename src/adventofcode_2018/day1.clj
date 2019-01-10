(ns adventofcode-2018.day1
  "Day 1 adventofcode"
  (:require [clojure.string :refer [split]]
            [clojure.spec.alpha :as s]))

(def input-str (-> "src/adventofcode_2018/day1.input"
                   slurp
                   (split #"\n")))

(def freq-deltas (map read-string input-str))
(comment (first freq-deltas))

(s/def ::freq-deltas (s/coll-of integer?))
(s/assert ::freq-deltas freq-deltas)

;; first part, get the first frequency reached twice

(def answer-1 (reduce + freq-deltas))

(defn process [state delta]
  (let [{:keys [history last-freq]} state
        new-freq (+ last-freq delta)
        present? (contains? history new-freq)
        next-state {:history (conj history new-freq) :last-freq new-freq}]
    (if present?
      (reduced next-state)
      next-state)))

(def answer-2 (reduce process {:history #{} :last-freq 0} (cycle freq-deltas)))
