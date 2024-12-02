(ns aoc2024.day2.part2
  (:require
   [aoc2024.day2.part1 :as part1]))

(def filename "src/aoc2024/day2/input.txt")

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
  (let [levels (part1/parse-input filename)]
    (->> levels
         (map bruteforce-safe-levels?)
         (filter true?)
         count)))

(comment
 (safe-count)) ;; 689

;; Failed attempt at a non-bruteforce approach
(comment
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
        (partition 3 1 (into [nil] levels))))))
