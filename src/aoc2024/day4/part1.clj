(ns aoc2024.day4.part1
  (:require
   [clojure.string :as str]))

(def filename "src/aoc2024/day4/input.txt")

(def input-lines (str/split-lines (slurp filename)))

(defn grid-nth
  [grid row col]
  (when (and (<= 0 row (dec (count grid)))
             (<= 0 col (dec (count (first grid)))))
    (let [line (nth grid row)]
      (nth line col))))

(defn search
  [grid row col row-step col-step search-string]
  (if-not (seq search-string)
    :XMAS
    (when (= (grid-nth grid row col) (first search-string))
      (search grid
              (row-step row)
              (col-step col)
              row-step
              col-step
              (rest search-string)))))

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

  (search grid 9 1 dec inc "XMAS"))

(defn solve
  [grid]
  (->>
   (for [row (range (count input-lines))
         col (range (count (first input-lines)))]
     (let [search-string "XMAS"]
       (count
        (filter some?
         [(search grid row col inc      inc      search-string)
          (search grid row col inc      identity search-string)
          (search grid row col inc      dec      search-string)
          (search grid row col identity inc      search-string)
          (search grid row col identity dec      search-string)
          (search grid row col dec      inc      search-string)
          (search grid row col dec      identity search-string)
          (search grid row col dec      dec      search-string)]))))
   (apply +)))

(comment
 (solve input-lines))
