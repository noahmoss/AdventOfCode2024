(ns aoc2024.day8.part1
  (:require
   [clojure.string :as str]
   [clojure.math.combinatorics :as combo]))

(def filename "src/aoc2024/day8/input.txt")

(defn parse-input
  []
  (-> (slurp filename)
      str/split-lines))

(def grid (parse-input))

(defn grid-nth
  [grid row col]
  (when (and (<= 0 row (dec (count grid)))
             (<= 0 col (dec (count (first grid)))))
    (let [line (nth grid row)]
      (nth line col))))

(defn candidates
  [[row1 col1] [row2 col2]]
  (let [row-step  (- row2 row1)
        col-step  (- col2 col1)
        next-pos  (fn [step-fn [row col]]
                    [(step-fn row row-step) (step-fn col col-step)])]
    (concat
     (take 1 (next (iterate (partial next-pos -) [row1 col1])))
     (take 1 (next (iterate (partial next-pos +) [row2 col2]))))))

(defn locations
  [grid]
  (apply merge-with concat
         (for [row (range (count grid))
               col (range (count (first grid)))]
           (let [c (grid-nth grid row col)]
             (when (not= c \.)
               {c [[row col]]})))))

(defn antinode-locations
  [grid]
  (distinct
   (mapcat
    (fn [antennas]
      (->> (combo/combinations antennas 2)
           (mapcat #(apply candidates %))
           (filter #(apply grid-nth grid %))))
    (vals (locations grid)))))

(comment
 (count (antinode-locations grid)))
