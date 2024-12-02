(ns aoc2024.day2.part2
  (:require
   [aoc2024.day2.part1 :as part1]
   [clojure.string :as str]))

(def filename "src/aoc2024/day2/input.txt")

(defn- parse-input
  [filename]
  (let [lines (str/split-lines (slurp filename))]
    (map
     (fn [line]
       (map parse-long (str/split line #" ")))
     lines)))

#_(defn- direction
    [level prev-level]
    (when prev-level
     (if (> level prev-level) :pos :neg)))

#_(defn- unsafe-pair?
    [level prev-level expected-dir]
    (if prev-level
      (boolean
       (let [difference (abs (- level prev-level))
             dir        (direction level prev-level)]
         (or (< difference 1)
             (> difference 3)
             (and expected-dir (not= expected-dir dir)))))
     false))

;; TODO: debug why this isn't working
#_(defn- safe-levels?
    "Reduce over triplets of elements from `levels`, returning a value early if an invariant is violated."
    [levels]
    (:safe?
     (reduce
      (fn [{expected-dir :dir
            unsafe-seen  :unsafe-seen?}
           [pprev prev curr]]
        (cond
          (unsafe-pair? curr prev expected-dir)
          (if (or unsafe-seen
                  (unsafe-pair? curr pprev expected-dir))
            (reduced {:safe? false})
            ;; We can make a safe pair by removing `prev` from the sequence. Continue reducing, but set `:unsafe-seen?` to
            ;; `true` so that the next unsafe pair aborts the reduction.
            {:safe?        true
             :unsafe-seen? true
             :dir          (direction curr pprev)})

          :else
          {:safe?        true
           :unsafe-seen? unsafe-seen
           :dir          (direction curr prev)}))
      {:safe? true
       :unsafe-seen? false}
      ;; Add a dummy `nil` head so that we start with the second level as `curr` in the first reduction
      (partition 3 1 (into [nil] levels)))))

(defn- bruteforce-safe-levels?
  [levels]
  (let [levels-with-one-removed
        (map
         (fn [i]
           (concat
            (take i levels)
            (drop (inc i) levels)))
         (range (count levels)))]
    (boolean (some part1/safe-levels? levels-with-one-removed))))

(defn safe-count
  []
  (let [levels (parse-input filename)]
    (->> levels
         (map bruteforce-safe-levels?)
         (filter true?)
         count)))

(comment
  (def levels
   [[7 6 4 2 1]
    [1 2 7 8 9]
    [9 7 6 2 1]
    [1 3 2 4 5]
    [8 6 4 4 1]
    [1 3 6 7 9]])
  (count
   (filter true? (map bruteforce-safe-levels? levels)))

  (bruteforce-safe-levels? [1 2 7 8 9])

  (partition 3 1 (into [nil] [1 5 2 4 5])))

(comment
 (partition 3 1 [1 2 3 4 5])
 (safe-count)) ;; 804
