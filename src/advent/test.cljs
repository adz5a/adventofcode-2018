(ns advent.test
  (:require [cljs.test :as t]
            [advent.core :as c]))


(t/deftest first-test
  (t/is (= 2 (inc 1))))



(t/run-tests)
