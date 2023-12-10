(ns aoc2023.day09
  (:require
    [clojure.string :as str]
    [aoc2023.util :as util]))

;;; day 9: Mirage Maintenance

(defn parse-input
  "Returns input as list of lists of the numbers."
  []
  (->> (util/get-input 9)
       #_"0 3 6 9 12 15\n1 3 6 10 15 21\n10 13 16 21 30 45"
       (str/split-lines)
       (util/parse-numbers)))

(defn find-all-diffs
  "Returns a list of lists, starting with nums itself and then all the
  diffs between the elements of nums etc as long as they're not zero."
  [nums]
  (loop [diffs nums
         all-diffs []]
    (if (every? zero? diffs)
      all-diffs
      (recur (map - (rest diffs) diffs)
             (conj all-diffs diffs)))))

;; part 1

(defn part-1
  []
  (transduce
    ; transform stuff:
    (comp (mapcat find-all-diffs) ; mapcat: ignore individual lines
          (map last))
    ; reduce stuff:
    + 0
    ; the stuff:
    (parse-input)))

;; part 2

(defn part-2
  []
  (transduce
    ; transform stuff:
    (comp (map find-all-diffs) ; get all diffs
          (map #(mapv first %)) ; get the first numbers
          (map rseq) ; need them reversed
          (map #(reduce (fn[a x] (- x a)) %))) ; predict before first
    ; reduce stuff:
    + 0
    ; the stuff:
    (parse-input)))
