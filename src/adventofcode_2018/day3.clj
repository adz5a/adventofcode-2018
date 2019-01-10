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

;; The idea is pretty simple but not really efficient from the complexity point
;; of view:
;; Had we had circles, detecting overlap would have been straightforward as it
;; is a mere comparison of distance between the center of each circles to the
;; sum of their respective radius.
;; We cannot really do this unless we use some kind of special norm
;; We say that rectangles are circles of radius 1 under the norm ||(x, y)|| =
;; max(|x|, |y|) with (x, y) normalised with (respectively) the width and the
;; height of the rectangles. We say that there is no overlap between two squares
;; S1 S2 if every corner of S2 is at distance d > 1 of the center of S1. The
;; center of S1 is the mean of the coordinates of each corners of S1.

(defn get-corners [claim]
  (let [top-left [(:left claim) (:top claim)]
        top-right [(+ (:width claim) (:left claim)) (:top claim)]
        bottom-left [(:left claim) (+ (:height claim) (:top claim))]
        bottom-right [(+ (:width claim) (:left claim)) (+ (:height claim) (:top claim))]]

    [top-left
     top-right
     bottom-left
     bottom-right]))

(comment (println (get-corners (first (parse-input-str test-data)))))

(defn abs
  "x -> |x|"
  [x]
  (max (- x) x))

(t/deftest test-abs
  (t/is (= 1 (abs -1))))

(defn get-center [claim]
  (let [corners (get-corners claim)]
    [(/ (reduce + 0 (map first corners)) 4)
     (/ (reduce + 0 (map second corners)) 4)]))

(comment
  (let [[claim] (parse-input-str test-data)]
      (get-center claim)))

(defn get-norm
  "Returns a function wich takes a point [x y]
  and returns its distance to the claim passed
  as argument. the distance is normalized with
  the size of the claim such as its bounds are
  at distance 1 of the center of the claim per
  this function."
  [claim]
  (let [{:keys [width height]} claim
        w (/ width 2)
        h (/ height 2)
        [cx cy] (get-center claim)]
    (fn dist-to-center [point]
      (let [[x y] point
            dx (abs (- x cx))
            dy (abs (- y cy))]
        (max (/ dx w)
             (/ dy h))))))

(defn overlaps?
  "tests id test-claim overlaps ref-claim. One
  claim is said to overlap another if one of its
  corners is at a normalized distance < 1 of the
  center of ref-claim"
  [ref-claim test-claim]
  (let [dist-to-ref (get-norm ref-claim)
        corners (get-corners test-claim)
        dists (map dist-to-ref corners)]
    (println dists)
    (some (partial > 1) dists)))

(comment
  (let [claims (parse-input-str test-data)]
    (for [x claims
          y claims
          :when (and
                  (not= x y)
                  (overlaps? x y))]
      [(:id x) (:id y)])))
