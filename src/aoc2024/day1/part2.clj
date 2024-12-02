(ns aoc2024.day1.part2
  (:require
   [clojure.string :as str]))

(def filename "src/aoc2024/day1/input2.txt")

(defn- parse-input
  [filename]
  (let [strings (-> (slurp filename)
                    (str/split #"\s+"))
        numbers (map parse-long strings)]
    [(take-nth 2 numbers)
     (take-nth 2 (rest numbers))]))

(defn- similarities
  [list1 list2]
  (let [list2-freqs (frequencies list2)]
    (map
     (fn [x]
       (* x (list2-freqs x 0)))
     list1)))

(defn total-similarity
  []
  (let [[list1 list2] (parse-input filename)]
   (reduce
    +
    (similarities list1 list2))))

(comment
 (total-similarity)) ;; 26407426
