(ns aoc2024.day6.part2
  (:require
   [aoc2024.day6.part1 :as part1]))

(def grid part1/grid)

(defn loop?
  "We're in a loop if we ever end up in the same position and direction we've been in before."
  [grid position dir new-barrier-position positions-set]
  (let [[new-row new-col :as new-position] (part1/step position dir)]
    (if (contains? positions-set [new-position dir])
      ;; found a loop!
      true
      (if-let [new-val (part1/grid-nth grid new-row new-col)]
        (if (or (= new-val \#)
                (= new-position new-barrier-position))
          ;; hit a barrier
          (let [new-dir (part1/rotate dir)]
            (recur grid
                   position
                   new-dir
                   new-barrier-position
                   (conj positions-set [position new-dir])))
          ;; no barrier, keep walking
          (recur grid
                 new-position
                 dir
                 new-barrier-position
                 (conj positions-set [new-position dir])))
        ;; exited the grid - no loop!
        false))))

(defn find-loops
  [grid]
  (let [guard-position (part1/guard-position grid)]
    (->> (for [[row col] (part1/all-positions grid guard-position :up #{guard-position})
               :when (not= [row col] guard-position)]
           (loop? grid guard-position :up [row col] #{[guard-position :up]}))
         (filter true?)
         count)))

(comment
 (time (find-loops grid)))
