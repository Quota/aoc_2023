(ns aoc2023.day03
  (:require
    [clojure.string :as str]
    [aoc2023.util :as util]))

;;; day 3: Gear Ratios

(defn char-symbol?
  [ch]
  (and (not= \. ch) (not (Character/isDigit ch))))

(defn collect-symbols
  "Returns a map with `symbols` and all symbols in the given `line`.
  Input: {...} r \"..#..\"
  Output: {..., [r 3] \\#, ...}"
  [symbols row line]
  (reduce-kv (fn [symbols col ch]
               (if (char-symbol? ch)
                 (assoc symbols [row col] ch)
                 symbols))
             symbols
             (vec line)))

(defn parse-number
  "Returns the number starting at `idx` in `line` (as string) or nil."
  [line idx]
  (loop [idx idx
         res nil]
    (let [ch (get line idx)]
      (if (and ch (Character/isDigit ch))
        (recur (inc idx) (str res ch))
        res))))

(defn find-number
  "Tries to find a number at `row` / `col` in `line`. If found returns
  the `numbers` map with the new number and the next col to check (i.e.
  after the number. Otherwise returns `numbers` and `(inc col)`.
  The return format is a vector like: [numbers next-col]."
  [numbers line row col]
  (let [num-str (parse-number line col)]
    (if num-str
      [(assoc numbers [row col] num-str) (+ col (.length num-str))]
      [numbers (inc col)])))

(defn collect-numbers
  "Collects numbers in the given `line`. Returns a map with `numbers`
  and all newly found numbers.
  Input: {...} r \"..#..nnn...*...\"
  Output: {[r c] \"nnn\", ..}"
  [numbers row line]
  (loop [col 0
         numbers numbers]
    (if (< col (.length line))
      (let [[numbers next-col] (find-number numbers line row col)]
        (recur next-col numbers))
      numbers)))

(defn parse-input
  "Returns a map with two maps, symbols and numbers.
  Output: {:symbols {[r c] \\s, ..}
           :numbers {[r c] \"nn..\", ..}"
  []
  (let [lines (->> (or #_"467..114..\n...*......\n..35..633.\n......#...\n617*......\n.....+.58.\n..592.....\n......755.\n...$.*....\n.664.598.."
                       (util/get-input 3))
                   (str/split-lines))]
    {:symbols (reduce-kv collect-symbols {} lines)
     :numbers (reduce-kv collect-numbers {} lines)}))

;; part 1

(defn part-number?
  "Returns if the `number` at `[row col]` is adjacent to a symbol."
  [sym-map [[row col] number]]
  (seq
    (filter sym-map
            (for [r (range (dec row) (+ row 2))
                  c (range (dec col) (+ col (.length number) 1))]
              [r c]))))

(defn part-1
  []
  (let [{:keys [symbols numbers]} (parse-input)]
    (->> numbers
         ; filter and keep only the part numbers
         (filter #(part-number? symbols %))
         ; then sum all part numbers up
         vals
         (map util/parse-int)
         (apply +))))

;; part 2

(defn interpolate-numbers
  "Fills the numbers map to cover all locations where digis exist
  with entries for their numbers.
  Input: {[2 7] \"500\", ...}
  Output: {[2 7] [[2 7] \"500\"],
           [2 8] [[2 7] \"500\"],
           [2 9] [[2 7] \"500\"],
           ...}"
  [numbers]
  (reduce-kv (fn[new-numbers [row col] n]
               (reduce #(assoc %1 %2 [[row col] n])
                       new-numbers
                       (for [c (range col (+ col (.length n)))] [row c])))
             {}
             numbers))

(defn get-gear-numbers
  "Returns the two gear numbers if there are any at [row col].
  Input: {[8 13] [[8 13] \"250\"],
  [8 14] [[8 13] \"250\"],
  [8 15] [[8 13] \"250\"],
  ...}
  Output: {[8 13] \"250\", ..} (if there are exactly two)."
  [interpolated-numbers [row col]]
  (let [; seq of all locations around [row col]
        rc-set (set (for [r (range (dec row) (+ row 2))
                          c (range (dec col) (+ col 2))]
                      [r c]))
        ; all numbers at any of these locations
        rc-nums (select-keys interpolated-numbers rc-set)
        ; "remove" interpolation of numbers
        rc-nums (into {} (vals rc-nums))
        ]
    (when (= 2 (count rc-nums))
      rc-nums)))

(defn part-2
  []
  (let [{:keys [symbols numbers]} (parse-input)
        asterisks (filter (fn[[_ v]] (#{\*} v)) symbols)
        numbers (interpolate-numbers numbers)]
    (->> asterisks
         keys
         (map #(get-gear-numbers numbers %))
         (remove nil?)
         (map (fn[m] (->> m vals (map util/parse-int) (apply *))))
         (apply +))))
