(ns aoc2023.day11
  (:require
    [clojure.string :as str]
    [clojure.math.combinatorics :as combo]
    [aoc2023.util :as util]))

;;; day 11: Cosmic Expansion

(defn parse-input
  "Returns input as map with keys :width, :height and :galaxies, the latter
  being a seqence of [row col] pairs."
  []
  (let [lines (str/split-lines (util/get-input 11))
        height (count lines)
        width (count (first lines))]
    {:width width
     :height height
     :galaxies (reduce-kv (fn[acc1 row line]
                            (reduce-kv (fn[acc2 col ch]
                                         (if (= ch \#) (conj acc2 [row col]) acc2))
                                       acc1 (vec line)))
                          #{} lines)}))

(defn expand-cols
  [growth galaxies col]
  (map (fn[[r c :as rc]] (if (> c col) [r (+ c growth)] rc)) galaxies))

(defn expand-rows
  [growth galaxies row]
  (map (fn[[r c :as rc]] (if (> r row) [(+ r growth) c] rc)) galaxies))

(defn expand-universe
  [{:keys [galaxies height width] :as universe} growth]
  (let [empty-rows (sort > (remove (set (map first galaxies)) (range height)))
        empty-cols (sort > (remove (set (map second galaxies)) (range width)))
        galaxies (reduce #(expand-cols growth %1 %2) galaxies empty-cols)
        galaxies (reduce #(expand-rows growth %1 %2) galaxies empty-rows)]
    ;(println "empty-rows:" empty-rows "and empty-cols:" empty-cols)
    {:width (+ width (count empty-cols))
     :height (+ height (count empty-rows))
     :galaxies galaxies}))

;; part 1

(defn analyze-observation
  [growth]
  (let [galaxy-pairs (-> (parse-input)
                         (expand-universe growth)
                         :galaxies
                         (combo/combinations 2))]
    (->> galaxy-pairs
         (map (fn[[g1 g2]] (apply + (map (comp abs -) g1 g2))))
         (reduce +))))

(defn part-1
  []
  (analyze-observation 1))

;; part 2

(defn part-2
  []
  (analyze-observation 999999))
