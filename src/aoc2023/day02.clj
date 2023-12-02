(ns aoc2023.day02
  (:require
    [clojure.string :as str]
    [aoc2023.util :as util]))

;;; day 2: Cube Conundrum

(defn add-rgb-vec
  "Input: [r g b] [\"ignored\" \"n\" \"<red|green|blue>\"]
  Output: [r g b] with either r, g or b increased by n"
  [rgb-vec [_ value col-name]]
  (let [col-idx ({"red" 0, "green" 1, "blue" 2} col-name)
        value (util/parse-int value)]
    (update rgb-vec col-idx + value)))

(defn parse-color-set
  "Input: \"n red, m green, k blue\"
  Output: [n m k]
  All colors in input are optional."
  [line]
  (->> line
       (re-seq #"(\d+) (red|green|blue)")
       (reduce add-rgb-vec [0 0 0])))

(defn parse-colors
  "Input: \"n red, m green; k green, p blue; ..\"
  Output: ([n m 0] [0 k p] ..)"
  [sets-line]
  (as-> sets-line $
    (str/split $ #" *; *")
    (map parse-color-set $)))

(defn parse-line
  "Input: 'Game i: n <red|green|blue>[, ..][; ..]'
  Output: {i [r g b], ..}"
  [line]
  (let [[_ game-nr sets-line] (re-find #"Game (\d+): (.*)" line)]
    [(util/parse-int game-nr)
     (parse-colors sets-line)]))

(defn parse-input
  "Returns input as map of game-nr to vec of max-rgb.
  Example: {1 [1 7 12], 2 [..], .. }"
  []
  (->> (util/get-input 2)
       (str/split-lines)
       (map parse-line)
       (map (fn[[nr rgb]] [nr (apply mapv max rgb)]))
       (into {})))

;; part 1

(defn part-1
  []
  (->> (parse-input)
       (filter (fn[[_ [r g b]]] (and (<= r 12) (<= g 13) (<= b 14))))
       ; or filter: (every? true? (map <= rgb [12 13 14]))
       keys
       (apply +)))

;; part 2

(defn part-2
  []
  (->> (parse-input)
       vals
       (map #(apply * %))
       (apply +)))
