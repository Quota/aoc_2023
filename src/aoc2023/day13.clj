(ns aoc2023.day13
  (:require
    [clojure.data :as data]
    [clojure.string :as str]
    [aoc2023.util :as util]))

;;; day 13: Point of Incidence

(defn parse-pattern
  "Returns a map with :size and :rocks with {:row n :col n}. While size
  has exactly one such map there's a set of them behind :rocks."
  [lines]
  {:size {:row (count lines) :col (count (first lines))}
   :rocks (reduce-kv (fn[acc1 row line]
                       (reduce-kv (fn[acc2 col ch]
                                    (if (= ch \#) (conj acc2 {:row row :col col}) acc2))
                                  acc1 (vec line)))
                     #{} (vec lines))})

(defn parse-input
  "Returns input as seq of maps with :size and :rocks with
  {:row n :col n} data. While size has exactly one such map
  there's a set of them behind :rocks."
  []
  (->> (util/get-input 13)
       (str/split-lines)
       (partition-by empty?)
       (take-nth 2)
       (map parse-pattern)))

;; part 1

(def invert {:col :row, :row :col})

(defn equals-roc?
  "Returns if two rows or columns are equal.
  Use `which` to select the coordinate (row or col) from the
  rock data (e.g. `:row` or `:col`).
  Then `i` and `j` are interpreted as row-index or col-index.
  ('roc' means 'row or column'.)"
  [rocks which i j]
  (let [other (invert which)]
    (= (set (map other (filter #(= i (which %)) rocks)))
       (set (map other (filter #(= j (which %)) rocks))))))

(defn mirrored-at?
  "Returns true if the given pattern is mirrored at the given axis `which`
  (:col or :row) and `nr` (the column or row number). The reflection will
  be checked right or below of the given column or row, respectively."
  [{:keys [size rocks]} which nr]
  (every?
    (fn[i] (equals-roc? rocks which (- nr i) (+ nr i 1)))
    (range (inc (min nr (- (which size) nr 2))))))

(defn find-reflection
  "Finds the reflection in the pattern using the mirror-fn.
  Returns a map with :col or :row and the column left to or
  row above the reflection axis, respectively.
  `mirror-fn` is called with three arguments: the pattern,
  :col or :row, and the column or row number, and it should
  return a truth value."
  ([mirror-fn pattern]
   ; the task uses 1-based rows and columns..
   (or (some->> (find-reflection mirror-fn :col pattern) inc (assoc {} :col))
       (some->> (find-reflection mirror-fn :row pattern) inc (assoc {} :row))))
  ([mirror-fn which {:keys [size rocks] :as pattern}]
   (if-let [refs (->> (range (dec (which size)))
                      (filter (fn[nr] (mirror-fn pattern which nr)))
                      seq)]
     (reduce max refs))))

(defn part-1
  []
  (let [refs (->> (parse-input)
                  (map (fn[pat] (find-reflection mirrored-at? pat))))]
    (+ (* 100 (reduce + (keep :row refs)))
       (reduce + (keep :col refs)))))

;; part 2

(defn count-diffs-roc
  "Returns in how many places two rows or columns differ.
  Use `which` to select the coordinate (row or col) from the
  rock data (e.g. `:row` or `:col`).
  Then `i` and `j` are interpreted as row-index or col-index.
  ('roc' means 'row or column'.)"
  [rocks which i j]
  (let [other (invert which)]
    (->> (data/diff
           (set (map other (filter #(= i (which %)) rocks)))
           (set (map other (filter #(= j (which %)) rocks))))
         (take 2)
         (map count)
         (reduce +))))

(defn mirrored-with-smudge?
  "Returns true if the given pattern is mirrored at the given axis `which`
  (:col or :row) and `nr` (the column or row number) with exactly one smudge.
  The reflection will be checked right or below of the given column or row,
  respectively."
  [{:keys [size rocks]} which nr]
  (->> (range (inc (min nr (- (which size) nr 2))))
       ;(map (fn[i] (count-diffs-roc rocks which (- nr i) (+ nr i 1))))
       ;(reduce +)
       (reduce (fn[smudges i]
                 (if (> smudges 1) ; fail early
                   (reduced smudges)
                   (+ smudges
                      (count-diffs-roc rocks which (- nr i) (+ nr i 1)))))
               0)
       (= 1)))

(defn part-2
  []
  (let [refs (->> (parse-input)
                  (map #(find-reflection mirrored-with-smudge? %)))]
    (+ (* 100 (reduce + (keep :row refs)))
       (reduce + (keep :col refs)))))
