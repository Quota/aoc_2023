(ns aoc2023.day10
  (:require
    [clojure.string :as str]
    [aoc2023.util :as util])
  (:import
    [java.awt Polygon]))

;;; day 10: Pipe Maze

(defn pipe->next-locs
  [tag row col]
  (case tag
    \| #{[(dec row) col] [(inc row) col] tag}
    \- #{[row (dec col)] [row (inc col)] tag}
    \F #{[(inc row) col] [row (inc col)] tag}
    \7 #{[(inc row) col] [row (dec col)] tag}
    \L #{[(dec row) col] [row (inc col)] tag}
    \J #{[(dec row) col] [row (dec col)] tag}
    ; start might connect to every location
    \S #{[(dec row) col] [(inc row) col] [row (dec col)] [row (inc col)] tag}))

(defn parse-lines
  [state row line] ; row = index, line = content
  (reduce-kv (fn[st col pipe] ; st = state-acc, col = index, pipe = F/J/...
               (let [st (if (= pipe \S)
                          (assoc st [:start] [row col])
                          st)]
                 (if (= pipe \.)
                   st
                   (assoc st [row col] (pipe->next-locs pipe row col)))))
             state
             (vec line)))

(defn fix-start-locations
  "Update the start locations to only contain valid locations."
  [state]
  (let [start-loc (state [:start])]
    (update state start-loc
            (fn[next-loc]
              (->> next-loc
                   (filter #(or (char? %) (get (state %) start-loc)))
                   set)))))

(defn parse-input
  "Returns input as map of locations [r c] to a set of conected locations.
  It will also contain the starting point.
  Output: {:start [rS cS], [r0 c0] #{[r1 c1] ..}, [r1 c1] #{[..] ..}"
  []
  (->> (util/get-input 10)
       (str/split-lines)
       (reduce-kv parse-lines (sorted-map))
       (fix-start-locations)))

;; part 1

(defn get-next-loc
  [area loc prev-loc]
  (let [next-loc (area loc)]
    (first (remove char? (remove #{prev-loc} next-loc)))))

; for plotting
(def graphs {\S \S
             \| \│ ; \u2502
             \- \─ ; \u2500
             \F \┌ ; \u250C
             \7 \┐ ; \u2510
             \L \└ ; \u2514
             \J \┘ }) ; \u2518

(defn plot-pipe
  [area path row col]
  (if-let [ch (first (filter char? (area [row col])))]
    (if (path [row col])
      ; highlight: S in red (91), path in bright white (90)
      (if (= \S ch)
        (str "\033[91m" (graphs ch) "\033[m")
        (str "\033[32m" (graphs ch) "\033[m"))
      ; normal
      (graphs ch))
    \ ))

(defn plot-str
  [area path]
  (let [ks (remove #{[:start]} (keys area))
        rows (range (inc (apply max (map first ks))))
        cols (range (inc (apply max (map second ks))))
        path (set path)]
    (reduce (fn[sb row]
              (.append
                (reduce (fn[sb col] (.append sb (plot-pipe area path row col))) sb cols)
                "\n"))
            (StringBuilder.)
            rows)))

(defn calc-path
  [area]
  (let [start-loc (area [:start])]
    (loop [path [start-loc]
           loc (get-next-loc area start-loc nil)
           prev-loc start-loc]
      (if (or (nil? loc) (= loc start-loc) (>= (count path) 100000))
        ; stop
        (do
          ; for ascii plotting enable the following line:
          ;(spit "var/plot-10.txt" (plot-str area path))
          {:path-length (count path)
           :path path})
        ; next location
        (recur (conj path loc)
               (get-next-loc area loc prev-loc)
               loc)))))

;; part 1

(defn part-1
  []
  (let [area (parse-input)
        path (:path (calc-path area))]
    (/ (count path) 2)))

;; part 2

; java.awt.Polygon has a nice method called 'contains' ;-)

(defn create-poly
  [path]
  (reduce (fn [p [r c]] (.addPoint p r c) p) (Polygon.) path))

(defn part-2
  []
  (let [area (parse-input)
        path (:path (calc-path area))
        poly (create-poly path)
        path (set path)]
    (->> (for [r (range 140) c (range 140)] [r c])
         (remove #{[:start]})
         (remove path)
         (filter (fn [[r c]] (.contains poly (double r) (double c))))
         count)))
