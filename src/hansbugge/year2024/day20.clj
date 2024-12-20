(ns hansbugge.year2024.day20
  (:require
   [hansbugge.utils :as utils]
   [medley.core :as m]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defonce input (utils/fetch-input {:year 2024 :day 20}))
(def test-input "###############\n#...#...#.....#\n#.#.#.#.#.###.#\n#S#...#.#.#...#\n#######.#.#.###\n#######.#.#...#\n#######.#.###.#\n###..E#...#...#\n###.#######.###\n#...###...#...#\n#.#####.#.###.#\n#.#...#.#.#...#\n#.#.#.#.#.#.###\n#...#...#...###\n###############")

(defn parse [input]
  (let [grid (utils/grid input)
        [start end] (reduce (fn [acc p]
                              (case (get-in grid p)
                                \S (assoc acc 0 p)
                                \E (assoc acc 1 p)
                                acc))
                            [nil nil]
                            (utils/points grid))]
    {:grid (-> grid
               (assoc-in start \.)
               (assoc-in end \.))
     :start start
     :end end}))

(defn path [{:keys [grid start end]}]
  (loop [path [start]]
    (let [p (peek path)]
      (if (= end p)
        path
        (let [next-point (->> [:n :s :e :w]
                              (map #(utils/step p %))
                              (remove #{(peek (pop path))})
                              (m/find-first #(= \. (get-in grid %))))]
          (recur (conj path next-point)))))))

(comment
  ;; length: (path includes start and end)
  (-> test-input parse path count dec)
  ;; => 84
  )

(defn shortcut-lengths [path ^long cheat-length]
  (for [^long i (range (count path))
        ^long j (range (inc i) (count path))
        :let [point-j (path j)]
        :let [dist (utils/manhattan-dist (path i) point-j)]
        :when (and (<= 2 dist cheat-length)
                   ;; don't include the one you would have walked to anyway
                   (not= point-j (path (+ i dist))))]
    (- j i dist)))

(defn part-1 [input]
  (let [path (path (parse input))]
    (->> (shortcut-lengths path 2)
         (filter #(<= 100 ^long %))
         count)))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 1948.705709 msecs"
  ;; => 1375
  )

(defn part-2 [input]
  (let [path (path (parse input))]
    (->> (shortcut-lengths path 20)
         (filter #(<= 100 ^long %))
         count)))

(comment
  (let [path (path (parse input))]
    (->> (shortcut-lengths path 20)
         (filter #(<= 50 %))
         frequencies
         sort))

  (time (part-2 input))
  ;; => 983054
  )
