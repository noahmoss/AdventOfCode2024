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
  "Map from page number to the set of page numbers that are only allowed to follow it"
  (update-vals
   (group-by first rules)
   (fn [rules]
     (set (map second rules)))))

(def updates
  (map
    (fn [update]
      (map parse-long (str/split update #",")))
    (rest (drop-while (complement str/blank?) input-lines))))

(defn filter-updates
  [updates find-valid?]
  (filter
   (fn [update]
     (loop [pages update
            preceded #{}]
        (if-not (seq pages)
          find-valid?
          (let [required-followers (followers (first pages))]
            (if-not (empty? (set/intersection required-followers preceded))
              (not find-valid?)
              (recur (rest pages) (conj preceded (first pages))))))))
   updates))

(defn sum-middles
  [updates]
  (apply +
         (map
          (fn [update] (nth update (/ (count update) 2)))
          (filter-updates updates true))))

(comment
 (sum-middles (filter-updates updates true)))
