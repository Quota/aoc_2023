(ns aoc2023.day05
  (:require
    [clojure.string :as str]
    [clojure.pprint :as pp]
    [aoc2023.util :as util]))

;;; day 5: If You Give A Seed A Fertilizer

(defn collect-maps
  [acc line]
  (let [[dst src len] (util/parse-numbers [:long] line)]
    (conj acc {:start src
               :end (+ src len -1) ; make incl, too
               :dest dst
               ;:length len
               })))

(comment
  (parse-maps ["1 2 3" "10 20 30"])
)

(defn parse-blocks
  [state [first-line & more-lines]]
  (if
    ; "seeds: ..." block
    (.startsWith first-line "seeds:")
    (assoc state :start {:dest :seed
                         :vals (util/parse-numbers [:long] (.substring first-line 7))})
    ; other blocks
    (let [[source _ dest _] (re-seq #"\w+" first-line)]
      (assoc state (keyword source) {:dest (keyword dest)
                                     :maps (reduce collect-maps [] more-lines)}))))

(comment
  (merge
  (parse-blocks {} ["seeds: 1 2 3"])
  (parse-blocks {} ["seed-to-soil map:" "10 50 5" "20 60 4" "30 70 3"])
  )
)

(defn parse-input
  "Returns input as map like:
  {:start{:dest seed :vals (n n ...)
   :seed {:dest soil :maps [{:start n :end n :dest n} ...]}
   :soil {:dest ...  :maps [...]}
   ...}"
  []
  (->> (util/get-input 5)
       #_(slurp "var/in-5-sample.txt")
       (str/split-lines)
       (partition-by empty?)
       (take-nth 2)
       (reduce parse-blocks {})))

(defn in-range?
  [value {:keys [start end] :as m}]
  (if (<= start value end)
    m nil))

(defn get-next-val
  [state source src-val]
  (if-let [maps (get-in state [source :maps])]
    (if-let [{:keys [start dest]} (reduce #(or %1 (in-range? src-val %2)) nil maps)]
      {:dest (get-in state [source :dest])
       :value (+ dest (- src-val start))}
      {:dest (get-in state [source :dest])
       :value src-val})
    nil))

(comment
  (get-next-val
    {:seed {:dest :soil :maps [{:start 20 :end 24 :dest 30}]}}
    :seed 21)
  ; -> {:dest :soil :value (31)}
  (get-next-val
    {:seed {:dest :soil :maps [{:start 20 :end 24 :dest 30}]}}
    :seed 27)
  ; -> {:dest :soil :value (27)}
)

;; part 1

(defn run-values
  [state dest0 value0]
  (if-let [{:keys [dest value]}
           (get-next-val state dest0 value0)]
    (run-values state dest value)
    value0))

(defn part-1
  []
  (time
    (let [state (parse-input)
          init-dest (get-in state [:start :dest])
          init-vals (get-in state [:start :vals])]
      (->> init-vals
           (map #(run-values state init-dest %))
           (apply min)))))

;; part 2

(defn part-2
  []
  (let [state (parse-input)]
    (count state)))
