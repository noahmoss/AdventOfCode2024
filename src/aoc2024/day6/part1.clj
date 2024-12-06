(ns aoc2024.day6.part1
  (:require
   [clojure.string :as str]))

(def filename "src/aoc2024/day6/input.txt")

(def data (slurp filename))

(def grid (str/split-lines data))

(defn grid-nth
  [grid row col]
  (when (and (<= 0 row (dec (count grid)))
             (<= 0 col (dec (count (first grid)))))
    (let [line (nth grid row)]
      (nth line col))))

(defn guard-position
  [grid]
  (first
   (keep-indexed
    (fn [row-idx row]
      (when-let [guard-col (str/index-of row \^)]
        [row-idx guard-col]))
    grid)))

(defn step-fns
  [dir]
  (case dir
    :up    [dec identity]
    :down  [inc identity]
    :left  [identity dec]
    :right [identity inc]))

(def rotate
  {:up    :right
   :right :down
   :down  :left
   :left  :up})

(defn step
  [[guard-row guard-col] dir]
  (let [[row-step col-step] (step-fns dir)]
    [(row-step guard-row) (col-step guard-col)]))

(defn all-positions
  [grid position dir positions-set]
  (let [[new-row new-col :as new-position] (step position dir)]
    (if-let [new-val (grid-nth grid new-row new-col)]
      (if (= new-val \#)
        (all-positions grid position (rotate dir) positions-set)
        (all-positions grid new-position dir (conj positions-set new-position)))
      positions-set)))

(comment
 (count (all-positions grid (guard-position grid) :up #{(guard-position grid)})))
