(ns aoc2023.day01
  (:require
    [clojure.string :as str]
    [aoc2023.util :as util]))

;;; day 1: Trebuchet?!

(defn parse-input
  "Returns input as seq of lines."
  []
  (->> (util/get-input 1)
       (str/split-lines)))

;; part 1

(defn extract-number-from-digits
  [line]
  (str/replace line #"^\D*(\d)(?:.*(\d))?\D*$"
               (fn[[_ d1 d2]] (str d1 (or d2 d1)))))

(defn part-1
  []
  (->> (parse-input)
       (map extract-number-from-digits)
       (map util/parse-int)
       (apply +)))

;; part 2

(defn part-2
  []
  (->> (parse-input)
       count))
