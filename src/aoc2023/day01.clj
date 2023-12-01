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
  "Returns the first and last digit of every line as 2-digit-number."
  [line]
  (-> line
      (str/replace #"^\D*(\d)(?:.*(\d))?\D*$" (fn[[_ g1 g2]] (str g1 (or g2 g1))))
      util/parse-int))

(defn part-1
  []
  (->> (parse-input)
       (map extract-number-from-digits)
       (apply +)))

;; part 2

(def WORD->DIGIT { "one" 1, "two" 2, "three" 3, "four" 4, "five" 5, "six" 6, "seven" 7, "eight" 8, "nine" 9 })

(defn extract-number-from-digits-and-words
  "Returns the first and last digit or digit-word of every line as
  2-digit-number."
  [line]
  (-> line
      (str/replace #"^.*?(\d|one|two|three|four|five|six|seven|eight|nine)(?:.*(\d|one|two|three|four|five|six|seven|eight|nine))?.*?$"
                   (fn[[_ g1 g2]]
                     (let [d1 (WORD->DIGIT g1 g1)
                           d2 (WORD->DIGIT g2 g2)]
                       (str d1 (or d2 d1)))))
      util/parse-int))

(defn part-2
  []
  (->> (parse-input)
       (map extract-number-from-digits-and-words)
       (apply +)))
