(ns adventofcode-2018.day3
  (:require [clojure.string :refer [split]]
            [clojure.spec.alpha :as s]
            [clojure.test :as t]))

(def test-data
"#1 @ 1,3: 4x4
#2 @ 3,1: 4x4
#3 @ 5,5: 2x2")

(def r #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+).*")

(s/def ::claim (s/cat
                 :id number?
                 :left number?
                 :top number?
                 :width number?
                 :height number?))

(defn parse-input-str [input]
  (as-> input %
    (split input #"\n")
    (map #(let [[_ & coords] (re-matches r %)
                coords (map read-string coords)]
            (s/conform ::claim coords))
         %)))

(t/deftest test-parse-input-str
  (t/is (let [out (parse-input-str test-data)]
          (every? (partial not= :clojure.spec.alpha/invalid)
                  out))
        "Every claim is correctly parsed"))

(def claims (parse-input-str test-data))

(defn get-pieces
  "Get all the pieces of a claim as a sequence of [int int]."
  [claim]
  (let [{:keys [left top width height]} claim]
    (for [x (range width)
          y (range height)]
      [(+ x left)
       (+ y top)])))

(defn get-overlapping-pieces
  "Returns the number of overlapping pieces."
  [claims]
  (let [r (reduce
            (fn [{:keys [board overlapping]} claim]
              (let [pieces (get-pieces claim)
                    board-size (count board)
                    overlapping-pieces (filter board pieces) ;; all pieces already in the board
                    next-board (apply conj board pieces) ;; the next board, regardless
                    next-overlapping-set (apply conj overlapping overlapping-pieces)]
                {:board next-board
                 :overlapping next-overlapping-set}))
            {:board #{} :overlapping #{}}
            claims)]
    (:overlapping r)))

(comment (get-overlapping-pieces claims))

(def data (slurp "src/adventofcode_2018/input.txt"))
(def real-claims (parse-input-str data))

(let [overlaps (get-overlapping-pieces real-claims)]
  (filter overlaps (get-pieces (first real-claims))))

(comment
  (let [overlappings (get-overlapping-pieces real-claims)]
    (loop [claim (first real-claims)
           claims (next real-claims)]
      (let [pieces (get-pieces claim)
            o (filter overlappings pieces)]
        (if (empty? o)
          (:id claim)
          (recur (first claims)
                 (next claims)))))))
