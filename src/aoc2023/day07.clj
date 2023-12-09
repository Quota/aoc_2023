(ns aoc2023.day07
  (:require
    [clojure.string :as str]
    [aoc2023.util :as util]))

;;; day 7: Camel Cards

(defn parse-line
  [line]
  (-> [:hand :bid]
      (zipmap (str/split line #" "))
      (update :bid util/parse-int)))

(defn parse-input
  "Returns input as list of maps with :hand and :bid."
  []
  (->> (util/get-input 7)
       #_"32T3K 765\nT55J5 684\nKK677 28\nKTJJT 220\nQQQJA 483"
       (str/split-lines)
       (map parse-line)))

(defn hand->type
  "Returns the type of hand. Assuming 5 cards (or less when there
  are jokers involved), one of the following will be returned:
  :five-of-a-kind, :four-of-a-kind, :full-house,
  :three-of-a-kind, :two-pairs, :one-pair, :high-card."
  [hand]
  (let [freq (sort > (vals (frequencies hand)))]
    (case freq
      ;0 joker   1 joker  2 j.   3 j.  4 j. 5 j.
      ((5)       (4)      (3)    (2)   (1)  ())  :five-of-a-kind
      ((4 1)     (3 1)    (2 1)  (1 1))          :four-of-a-kind
      ((3 2)     (2 2))                          :full-house
      ((3 1 1)   (2 1 1)  (1 1 1))               :three-of-a-kind
      ((2 2 1))                                  :two-pairs
      ((2 1 1 1) (1 1 1 1))                      :one-pair
      ((1 1 1 1 1))                              :high-card)))

(def type->rank
  {:five-of-a-kind 6 :four-of-a-kind 5 :full-house 4
   :three-of-a-kind 3 :two-pairs 2 :one-pair 1 :high-card 0})

;; part 1

(defn hand->comparable
  "Convert T, J, Q, K and A to a, b, c, d and e, respectively.
  Leave everything else as it was."
  [hand]
  (mapv #({\T \a \J \b \Q \c \K \d \A \e} % %) hand))

(defn part-1
  []
  (->> (parse-input)
       ; sort first by the `hand`, then by the `type` (b/c sort is stable
       ; the result is primarily sorted by `type` and then by `hand`)
       (sort-by (comp hand->comparable :hand))
       (sort-by (comp type->rank hand->type :hand))
       ; calculate the win value
       (map-indexed (fn[i hand] (* (inc i) (:bid hand))))
       (apply +)))

;; part 2

(defn hand->comparable-with-joker
  "Convert T, Q, K and A to a, b, c, and d, respectively, and
  J to 1 (i.e. lowest). Leave everything else as it was."
  [hand]
  (mapv #({\T \a \J \1 \Q \c \K \d \A \e} % %) hand))

(defn part-2
  []
  (->> (parse-input)
       ; sort first by the `hand`, then by the `type` (b/c sort is stable
       ; the result is primarily sorted by `type` and then by `hand`)
       (sort-by (comp hand->comparable-with-joker :hand))
       (sort-by (comp type->rank hand->type (partial remove #{\J}) :hand))
       ; calculate the win value
       (map-indexed (fn[i hand] (* (inc i) (:bid hand))))
       (apply +)))
