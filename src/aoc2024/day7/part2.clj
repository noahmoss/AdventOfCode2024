(ns aoc2024.day7.part2
  (:require
   [aoc2024.day7.part1 :as part1]))

(def inputs (part1/parse-input))

(defn ||
  [x y]
  (when (and x y) (parse-long (str x y))))

(defn solve
  []
  (->> inputs
       (keep (fn [[target nums]] (part1/equals? target nums #{+ * ||})))
       (reduce +)))

(comment
 (solve))
