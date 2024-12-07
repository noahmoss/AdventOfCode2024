(ns aoc2024.day7.part1
  (:require
   [clojure.string :as str]))

(def filename "src/aoc2024/day7/input.txt")

(defn parse-input
  []
  (let [lines (str/split-lines (slurp filename))]
    (map
     (fn [line]
       (let [parsed-line (->> (str/split line #":?\s+")
                              (map parse-long))]
         [(first parsed-line) (rest parsed-line)]))
     lines)))

(def inputs (parse-input))

(defn equals?
  ([target nums ops]
   (equals? target (first nums) (rest nums) ops))

  ([target total [first-num & rest-nums] ops]
   (if (nil? first-num)
     (and (= target total) total)
     (some identity
           (map
            #(equals? target (% total first-num) rest-nums ops)
            ops)))))

(defn solve
  []
  (->> inputs
       (keep (fn [[target nums]] (equals? target nums #{+ *})))
       (reduce +)))

(comment (solve))

