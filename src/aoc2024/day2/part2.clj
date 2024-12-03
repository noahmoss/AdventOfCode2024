(ns aoc2024.day2.part2
  (:require
   [aoc2024.day2.part1 :as part1]))

(def filename "src/aoc2024/day2/input.txt")

(defn- bruteforce-safe-levels?
  [levels]
  (let [levels-with-one-removed
        (map
         (fn [i]
           (concat (take i levels)
                   (drop (inc i) levels)))
         (range (count levels)))]
    (boolean (some part1/safe-levels? levels-with-one-removed))))

(defn safe-count
  []
  (let [levels (part1/parse-input filename)]
    (->> levels
         (map bruteforce-safe-levels?)
         (filter true?)
         count)))

(comment
 (safe-count)) ;; 689
