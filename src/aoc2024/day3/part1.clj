(ns aoc2024.day3.part1)

(def filename "src/aoc2024/day3/input.txt")

(def data (slurp filename))

(def muls (re-seq #"mul\((\d{1,3}),(\d{1,3})\)" data))

(def products
 (map
  (fn [[_, s1, s2]]
    (* (parse-long s1) (parse-long s2)))
  muls))

(reduce + products)

;; part 2

(def parsed-ops (re-seq #"mul\((\d{1,3}),(\d{1,3})\)|do\(\)|don't\(\)" data))

(:sum
 (reduce
  (fn [{:keys [enabled sum] :as acc} [operation, s1, s2]]
    (case operation
      "don't()" {:enabled false :sum sum}
      "do()" {:enabled true :sum sum}
      (if enabled
        {:enabled enabled :sum (+ sum (* (parse-long s1) (parse-long s2)))}
        acc)))
  {:enabled true
   :sum 0}
  parsed-ops))
