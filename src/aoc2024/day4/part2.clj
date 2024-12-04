(ns aoc2024.day4.part2
  (:require [aoc2024.day4.part1 :as part1]))

(def input-lines part1/input-lines)

(defn search
  [grid row col]
  (when
    (and
     (= (part1/grid-nth grid row col) \A)
     (= #{\M \S}
        (into #{}
              [(part1/grid-nth grid (inc row) (inc col))
               (part1/grid-nth grid (dec row) (dec col))])
        (into #{}
              [(part1/grid-nth grid (inc row) (dec col))
               (part1/grid-nth grid (dec row) (inc col))])))
    :X-MAS))

(defn solve
  [grid]
  (->>
   (for [row (range (count grid))
         col (range (count (first grid)))]
    (search grid row col))
   (filter some?)
   count))

(comment
  (def grid
    ["MMMSXXMASM"
     "MSAMXMSMSA"
     "AMXSXMAAMM"
     "MSAMASMSMX"
     "XMASAMXAMM"
     "XXAMMXXAMA"
     "SMSMSASXSS"
     "SAXAMASAAA"
     "MAMMMXMMMM"
     "MXMXAXMASX"])
  (solve grid))

(comment
 (solve input-lines))
