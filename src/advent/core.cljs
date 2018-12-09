(ns advent.core
  (:require [cljs.nodejs :as nodejs]
            [clojure.string :as string]
            [clojure.pprint :as pprint]))

(nodejs/enable-util-print!)

(def cwd (.cwd nodejs/process))
(def fs (nodejs/require "fs"))
(def input (.readFileSync fs (str cwd "/input.txt") "utf8"))

(def data (->> input
               string/split-lines
               (map #(string/split % #""))))

;; compute the counts for the first exercise
(def counts (->> data
               (map #(->> %
                          frequencies
                          (filter (fn [[_ f]]
                                    (>= f 2)))
                          (group-by second)))
               (reduce (fn [totals m]
                         (-> totals
                             (update 3 + (if (contains? m 3)
                                           1
                                           0))
                             (update 2 + (if (contains? m 2)
                                           1
                                           0))))
                       {3 0
                        2 0})))

(pprint/pprint counts)
