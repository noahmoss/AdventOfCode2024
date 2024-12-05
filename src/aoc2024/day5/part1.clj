(ns aoc2024.day5.part1
  (:require
   [clojure.set :as set]
   [clojure.string :as str]))

(def filename "src/aoc2024/day5/input.txt")

(def input-lines (str/split-lines (slurp filename)))

(def rules
 (map
  (fn [rule]
    (map parse-long (str/split rule #"\|")))
  (take-while (complement str/blank?) input-lines)))

(def followers
  "Map from page number to the set of page numbers that cannot precede it"
  (update-vals
   (group-by first rules)
   (fn [rules]
     (set (map second rules)))))

(def updates
  (map
    (fn [update]
      (map parse-long (str/split update #",")))
    (rest (drop-while (complement str/blank?) input-lines))))

(def valid-updates
 (filter
  (fn [update]
    (loop [pages update
           preceded #{}]
       (if-not (seq pages)
         true
         (let [required-followers (followers (first pages))]
           (if-not (empty? (set/intersection required-followers preceded))
             false
             (recur (rest pages) (conj preceded (first pages))))))))
  updates))

(apply +
 (map
  (fn [update]
    (nth update (/ (count update) 2)))
  valid-updates))
