(ns aoc2023.day08
  (:require
    [clojure.string :as str]
    [aoc2023.util :as util]))

;;; day 8: Haunted Wasteland

(defn parse-line
  [state line]
  (cond
    ; first line: directions
    (nil? (:directions state)) (assoc state :directions line)
    ; second line: empty (skip)
    (empty? line) state
    ; rest: network definition
    :rest (let [[node left right] (re-seq #"\w+" line)]
            (update state :network
                    assoc node {\L left \R right}))))

(defn parse-input
  "Returns input as a map with :directions and :network. The directions
  is the LR string from the input. The network is a map of nodes to
  the next-nodes, like this: {\"AAA\" {\\L \"BBB\", \\R \"CCC\"}, ...}."
  []
  (->> (util/get-input 8)
       (str/split-lines)
       (reduce parse-line {})))

(defn navigate-one-step
  "Traverses one step in the network.
  Input: network-map stop-fn [from-node step-count-thus-far] <\\L or \\R>
  Output: [to-node step-count-inc'ed]"
  [network stop-fn [node step :as curr] direction]
  (if (stop-fn node)
    curr
    [(get-in network [node direction]) (inc step)]))

(defn navigate-till
  "Traverses all directions across the network, starting at `start-node`,
  repeating the directions until `stop-fn` returns true.
  The last node and step cound will be returned (within a vec)."
  [{:keys [network directions]} start-node stop-fn]
  (loop [[node step :as curr] [start-node 0]]
    (if (or (stop-fn node) (> step 100000)) ; safety check: stop at 100,000
      curr
      (recur (reduce #(navigate-one-step network stop-fn %1 %2) curr directions)))))

;; part 1

(defn part-1
  []
  (navigate-till (parse-input) "AAA" #(= "ZZZ" %)))

;; part 2

(defn part-2
  []
  (let [input (parse-input)
        a-nodes (filter #(.endsWith % "A") (keys (:network input)))
        z-nodes (map (fn[node] (conj (navigate-till input node #(.endsWith % "Z")) node)) a-nodes)
        all-z-at (->> z-nodes (map second) (reduce util/lcm))]
    {:all-z-at all-z-at :z-nodes z-nodes}))
