(ns aoc2024.day4.part2
  (:require [aoc2024.day4.part1 :as part1]))

(def input-lines part1/input-lines)

(defn search
  [grid row col]
  (if
    (and
     (= (part1/grid-nth grid row col) \A)
     (= #{\M \S}
        (set [(part1/grid-nth grid (inc row) (inc col))
              (part1/grid-nth grid (dec row) (dec col))])
        (set [(part1/grid-nth grid (inc row) (dec col))
              (part1/grid-nth grid (dec row) (inc col))])))
    1
    0))

(defn solve
  [grid]
  (apply +
   (for [row (range (count grid))
         col (range (count (first grid)))]
    (search grid row col))))

(comment
 (solve input-lines))
