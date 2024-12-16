(ns hansbugge.year2024.day16
  (:require
   [hansbugge.utils :as utils]
   [medley.core :as m]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defonce input (utils/fetch-input {:year 2024 :day 16}))
(def test-input "###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############")

(defn parse [input]
  (let [grid (utils/grid input)
        points (utils/points grid)
        start (m/find-first #(= \S (get-in grid %)) points)
        end (m/find-first #(= \E (get-in grid %)) points)
        graph (reduce
               (fn [graph p]
                 (case (get-in grid p)
                   \# graph
                   (\. \E \S) (reduce
                               (fn [graph dir]
                                 (let [node [p dir]
                                       step-node [(utils/step p dir) dir]
                                       cwise  [p (utils/clockwise dir)]
                                       ccwise [p (utils/counter-clockwise dir)]]
                                   (-> graph
                                       ;; add the two possible turns as nodes in the graph
                                       ;; with edge weights 1000
                                       (update node (fnil conj #{}) [cwise 1000])
                                       (update node conj [ccwise 1000])
                                       ;; if we can walk forward, add this as a node to the graph
                                       ;; with edge weight 1
                                       (cond-> (#{\. \E \S} (get-in grid (first step-node)))
                                         (update node conj [step-node 1])))))
                               graph
                               [:n :s :e :w])))
               {}
               (utils/points grid))]
    {:graph graph
     :start [start :e]
     :targets (into #{} (map (fn [dir] [end dir])) [:n :s :e :w])}))

(defn part-1 [input]
  (let [{:keys [graph start targets]} (parse input)]
    (-> (utils/dijkstra graph start targets)
        first
        long)))

(comment
  (part-1 test-input)
  ;; => 7036
  (time (part-1 input))
  ;; "Elapsed time: 288.6605 msecs"
  ;; => 105496
  )

(defn part-2 [input]
  (let [{:keys [graph start targets]} (parse input)]
    (count (into #{}
                 (comp cat (map first))
                 (second (utils/dijkstra graph start targets))))))

(comment

  (part-2 test-input)
  ;; => 45
  (time (part-2 input))
  ;; "Elapsed time: 284.08975 msecs"
  ;; => 524
  )
