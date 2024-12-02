(ns aoc2024.day2.part1
  (:require
   [clojure.string :as str]))

(def filename "src/aoc2024/day2/input.txt")

(defn- parse-input
  [filename]
  (let [lines (str/split-lines (slurp filename))]
    (map
     (fn [line]
       (map parse-long (str/split line #" ")))
     lines)))

(defn safe-levels?
  "Reduce over pairwise elements from `levels`, returning a value early if an invariant is violated."
  [levels]
  (:safe?
   (reduce
    (fn [{expected-dir :dir} [last-level level]]
      (let [difference (abs (- level last-level))
            direction  (if (> level last-level) :pos :neg)]
        (cond
          ;; Are the current pair too far apart?
          (or (< difference 1)
              (> difference 3))
          (reduced {:safe? false})

          ;; Are the current pair going the wrong direction?
          (and expected-dir
               (not= expected-dir direction))
          (reduced {:safe? false})

          :else
          {:safe? true
           :dir   direction})))
    {:safe? true}
    (partition 2 1 levels))))

(defn safe-count
  []
  (let [levels (parse-input filename)]
    (->> levels
         (map safe-levels?)
         (filter true?)
         count)))

(comment
 (safe-count)) ;; 660
