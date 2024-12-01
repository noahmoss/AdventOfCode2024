(ns aoc2024.day1.part1
  (:require
   [clojure.string :as str]))

(def filename "src/aoc2024/day1/input1.txt")

(defn- parse-input
  [filename]
  (let [strings (-> (slurp filename)
                    (str/split #"\s+"))
        numbers (map parse-long strings)]
    [(take-nth 2 numbers)
     (take-nth 2 (rest numbers))]))

(defn- distances
  [list1 list2]
  (let [list1 (sort list1)
        list2 (sort list2)]
    (map
     (fn [val1 val2]
       (abs (- val1 val2)))
     list1
     list2)))

(defn total-distance
  []
  (let [[list1 list2] (parse-input filename)
        distances     (distances list1 list2)]
    (reduce + distances)))

(comment
 (total-distance)) ;; 3569916
