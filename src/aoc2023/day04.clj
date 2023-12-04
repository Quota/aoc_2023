(ns aoc2023.day04
  (:require
    [clojure.string :as str]
    [aoc2023.util :as util]))

;;; day 4: Scratchcards

(defn parse-line
  "Returns a vec with the winning numbers and my numbers, both as sets.
  Output: [#{n ..} ; win numbers
           #{n ..}] ; my numbers"
  [line]
  (let [[game-ignored & numbers] (str/split line #" *[:|] *")]
    (->> numbers
         (util/parse-numbers)
         (mapv set))))

(defn parse-input
  "Returns a seq of all games in the form of `parse-line` return values."
  []
  (->> (util/get-input 4)
       (str/split-lines)
       (map parse-line)))

;; part 1

(defn count-winning-cards
  "Returns the count of my winning numbers."
  [[win-numbers my-numbers]]
  (count (filter win-numbers my-numbers)))

(defn calc-points
  "Calculates the points according to the given winning number count."
  [win-count]
  (if (zero? win-count) 0 (bit-shift-left 1 (dec win-count))))

(defn part-1
  []
  (->> (parse-input)
       (map count-winning-cards)
       (map calc-points)
       (apply +)))

;; part 2

(defn collect-wins
  "Updates `win-count` values in `card-counts` after `game-nr` by adding
  the value of `card-counts` at `game-nr`."
  [card-counts game-nr win-count]
  (let [game-nr-card-count (card-counts game-nr)
        game-nr-end (+ game-nr win-count 1)]
    (->> card-counts
         ; update `card-counts` at all indexes from `game-nr` (excl.) and
         ; `(+ game-nr win-count)` (incl.) by adding the number of cards
         ; at game-nr (i.e. `(get card-counts game-nr)`)
         (map-indexed (fn [i card-count]
                        (if (< game-nr i game-nr-end)
                          (+ card-count game-nr-card-count)
                          card-count)))
         vec)))

(defn part-2
  []
  (let [game->win-count (mapv count-winning-cards (parse-input))]
    ; aggregate all game's win-counts, starting at 1 for every game
    ; (i.e. vector [1 1 ... 1])
    (->> (reduce-kv collect-wins
                    ; inital vec, with as many 1's as there are games
                    (-> game->win-count count (repeat 1) vec)
                    game->win-count)
         (apply +))))
