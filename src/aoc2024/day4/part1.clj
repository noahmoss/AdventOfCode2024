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
    1
    (when (= (grid-nth grid row col) (first search-string))
      (search grid
              (row-step row)
              (col-step col)
              row-step
              col-step
              (rest search-string)))))

(defn solve
  [grid]
  (let [search-string "XMAS"]
    (->>
      (for [row (range (count input-lines))
            col (range (count (first input-lines)))
            row-fn [inc dec identity]
            col-fn [inc dec identity]
            :when (not= [row-fn col-fn] [identity identity])]
        (search grid row col row-fn col-fn search-string))
      (remove nil?)
      (apply +))))

(comment
 (solve input-lines))
