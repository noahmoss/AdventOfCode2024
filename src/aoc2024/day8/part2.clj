(ns aoc2024.day8.part2
  (:require
   [aoc2024.day8.part1 :as part1]))

(defn diag
  [x y]
  (Math/sqrt (+ (Math/pow x 2) (Math/pow y 2))))

(defn candidates
  [grid [row1 col1] [row2 col2]]
  (let [row-step  (- row2 row1)
        col-step  (- col2 col1)
        next-pos  (fn [step-fn [row col]]
                    [(step-fn row row-step) (step-fn col col-step)])
        grid-diag (diag (count grid) (count (first grid)))
        max-steps (/ grid-diag (diag row-step col-step))]
    (concat
     (take max-steps (iterate (partial next-pos -) [row1 col1]))
     (take max-steps (iterate (partial next-pos +) [row2 col2])))))

(comment
 (count (part1/antinode-locations part1/grid candidates)))
