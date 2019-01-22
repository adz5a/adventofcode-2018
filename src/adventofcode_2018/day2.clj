(ns adventofcode-2018.day2
  "Day 2 adventofcode"
  (:require [clojure.string :refer [split]]))


(def box-ids (as-> "src/adventofcode_2018/day2.input" f
             (slurp f)
             (split f #"\n")
             (map seq f)))

(comment (pprint (frequencies (first input))))
(comment (seq (frequencies (first input))))

(defn get-counts [char-seq]
  (loop [char-frequencies (frequencies char-seq)
         counts {:3 0 :2 0}]
    (if (empty? char-frequencies)
      counts
      (recur (rest char-frequencies)
             (let [[c f] (first char-frequencies)]
               (case f
                 2 (assoc counts :2 1) ;; when a match is found, it counts exactly once per box-id 
                 3 (assoc counts :3 1)
                 counts))))))

(comment
  (frequencies (first box-ids))
  (get-counts (first box-ids)))

(def checksum (let [counts (reduce
                             (fn [s c]
                               {:3 (+ (:3 s) (:3 c))
                                :2 (+ (:2 s) (:2 c))})
                             (map get-counts box-ids))]
                (* (:3 counts) (:2 counts))))

(comment
  (pprint checksum))

;; We need to find the two strings among the box-ids sequence that differs only
;; with one char
;; One approach could be: for each string, compare to all others until you
;; either find your match or you run out of string.
;; Comparing two strings side by side is not hard and counting the number of
;; differences is straightforward.
(defn count-diffs [seq1 seq2]
  (reduce + 0 (map #(if (= %1 %2) 1 0) seq1 seq2)))

(comment (count-diffs (first box-ids)
                      (second box-ids)))
