(ns aoc2024.day5.part2
  (:require
   [aoc2024.day5.part1 :as part1]))

(def invalid-updates
  (part1/filter-updates part1/updates false))

(defn must-precede?
  [x y]
  (let [x-followers (part1/followers x)]
    (boolean (x-followers y))))

(defn sort-updates
  [invalid-updates]
  (map
   (fn [update]
     (sort-by identity must-precede? update))
   invalid-updates))

(comment
 (part1/sum-middles (sort-updates invalid-updates)))
