(ns hansbugge.year2024.day6
  (:require
   [hansbugge.utils :as utils]
   [medley.core :as m]
   [net.cgrand.xforms :as xf]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

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
          c (get-in g nxt)
          dir (first dirs)]
      (cond
        (nil? c) (keys visited)
        (= \# c) (recur (rest dirs) pnt visited)
        (get-in visited [nxt dir]) :loop
        :else (recur dirs nxt (update visited nxt (fnil conj #{}) dir))))))

(defn find-start-point [g]
  (->> (utils/points g) (m/find-first #(= \^ (get-in g %)))))

(defn part-1 [input]
  (let [g (utils/grid input)]
    (count (walk g (find-start-point g)))))

(part-1 test-input)
;; => 41
(part-1 input)
;; => 5162

(defn part-2 [input]
  (let [g (utils/grid input)
        start-point (find-start-point g)
        path (walk g start-point)]
    (xf/count (comp
               (remove #{start-point})
               (map (fn [pnt] (walk (assoc-in g pnt \#) start-point)))
               (filter #{:loop}))
              path)))

(comment
  (part-2 test-input)
  ;; => 6
  (time (part-2 input))
  ;; "Elapsed time: 5727.726542 msecs"
  ;; => 1909
  )

(defn part-2-parallel [input ^long parallelism]
  (let [g (utils/grid input)
        start-point (find-start-point g)
        path (walk g start-point)]
    (->> path
         (remove #{start-point})
         (partition-all (quot (count path) parallelism))
         (pmap (fn [batch]
                 (xf/count (comp
                            (map (fn [pnt] (walk (assoc-in g pnt \#) start-point)))
                            (filter #{:loop}))
                           batch)))
         (apply +))))

(comment
  (doseq [p (range 1 12)]
    (println "With paralellism:" p)
    (time (part-2-parallel input p))
    (println))
  ;;  With paralellism: 1
  ;;  "Elapsed time: 5921.958875 msecs"
  ;;
  ;;  With paralellism: 2
  ;;  "Elapsed time: 3067.896959 msecs"
  ;;
  ;;  With paralellism: 3
  ;;  "Elapsed time: 2090.957875 msecs"
  ;;
  ;;  With paralellism: 4
  ;;  "Elapsed time: 1588.256625 msecs"
  ;;
  ;;  With paralellism: 5
  ;;  "Elapsed time: 1285.381667 msecs"
  ;;
  ;;  With paralellism: 6
  ;;  "Elapsed time: 1114.890417 msecs"
  ;;
  ;;  With paralellism: 7
  ;;  "Elapsed time: 982.610959 msecs"
  ;;
  ;;  With paralellism: 8
  ;;  "Elapsed time: 876.205083 msecs"
  ;;
  ;;  With paralellism: 9
  ;;  "Elapsed time: 848.145708 msecs"
  ;;
  ;;  With paralellism: 10
  ;;  "Elapsed time: 820.716834 msecs"
  ;;
  ;;  With paralellism: 11
  ;;  "Elapsed time: 824.9345 msecs"

  ;; So the winner so far is:
  (time (part-2-parallel input 10))
  ;; "Elapsed time: 824.259834 msecs"
  ;; => 1909
  )
