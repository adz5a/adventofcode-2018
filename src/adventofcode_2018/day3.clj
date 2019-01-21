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

(defn count-overlapping
  "Returns the number of overlapping pieces."
  [claims]
  (let [r (reduce
            (fn [{:keys [board nb]} claim]
              (let [pieces (get-pieces claim)
                    board-size (count board)
                    nb-pieces (count pieces)
                    expected-size (+ board-size nb-pieces)
                    next-board (apply conj board pieces)
                    overlapping-pieces (- expected-size (count next-board))]
                {:board next-board
                 :nb (+ nb overlapping-pieces)}))
            {:board #{}
             :nb 0}
            claims)]
    (:nb r)))

(comment (count-overlapping claims))

(def data (slurp "src/adventofcode_2018/input.txt"))
(def real-claims (parse-input-str data))

(comment
  (println data)
  (println real-claims)
  (println (count-overlapping (take 10 real-claims))))
