(ns hansbugge.year2024.day6
  (:require
   [hansbugge.utils :as utils]))

(defonce input (utils/fetch-input {:year 2024 :day 6}))
(def test-input "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...")

(defn walk [g start-point]
  (loop [dirs (cycle [:n :e :s :w])
         pnt start-point
         visited {start-point #{:n}}]
    (let [nxt (utils/step pnt (first dirs))
          c (get-in g nxt)]
      (cond
        (nil? c) (keys visited)
        (= \# c) (recur (rest dirs) pnt visited)
        (get-in visited [nxt (first dirs)]) :loop
        :else (recur dirs nxt (update visited nxt (fnil conj #{}) (first dirs)))))))

(defn part-1 [input]
  (let [g (utils/grid input)
        start-point (->> (utils/points g)
                         (filter #(= \^ (get-in g %)))
                         first)]
    (count (walk g start-point))))

(part-1 test-input)
;; => 41
(part-1 input)
;; => 5162

(defn part-2 [input]
  (let [g (utils/grid input)
        start-point (->> (utils/points g)
                         (filter #(= \^ (get-in g %)))
                         first)
        path (walk g start-point)]
    (->> path
         (remove #{start-point})
         (map (fn [pnt] (walk (assoc-in g pnt \#) start-point)))
         (filter #{:loop})
         count)))

(comment
  (part-2 test-input)
  ;; => 6
  (time (part-2 input))
  ;; "Elapsed time: 5916.123833 msecs"
  ;; => 1909
  )
