(ns aoc2023.day06
  (:require
    [clojure.string :as str]
    [aoc2023.util :as util]))

;;; day 6: Wait For It (Boat Races!)

(defn parse-input
  "Returns input as seq of pairs [time distance], or just the one pair 
  in case of `ignore-spaces?`."
  [ignore-spaces?]
  (let [lines-with-numbers (->> (util/get-input 6)
                                (str/split-lines)
                                (map #(->> % seq (drop-while (complement #{\ })) (apply str) .trim)))]
    (if ignore-spaces?
      (->> lines-with-numbers
           (map #(str/replace % #" +" ""))
           (util/parse-numbers [:long])
           (mapcat identity))
      (->> lines-with-numbers
           (util/parse-numbers)
           (apply map (fn[a b] [a b]))))))

;; part 1

(defn solve-quad
  "Solve the quadratic equation. Using the quadratic formula this fn returns
  a sorted vec with the two solutions for x to `a * x * x + b * x + c = 0`.
  Note that this function does NOT check for zero or one solutions, it expects
  to always find two solutions."
  [a b c]
  (let [r (Math/sqrt (- (* b b) (* 4 a c)))]
    (sort [(/ (- (+ b r)) (* 2 a)) (/ (- r b) (* 2 a))])))

(def EPSILON 1e-9)

(defn find-min-max-charge-time
  [total-time record]
  (let [[y1 y2] (solve-quad -1 total-time (- record))
        min-charge (int (Math/ceil (+ y1 EPSILON)))
        max-charge (int (- y2 EPSILON))]
    [min-charge max-charge (- max-charge min-charge -1)]))
  
(defn part-1
  []
  (->> (parse-input false)
       (map #(apply find-min-max-charge-time %))
       (map last)
       (apply *)))

;; part 2

(defn part-2
  []
  (->> (parse-input true)
       (apply find-min-max-charge-time)
       last))
