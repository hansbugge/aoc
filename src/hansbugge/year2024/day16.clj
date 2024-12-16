(ns hansbugge.year2024.day16
  (:require
   [clojure.data.priority-map :refer [priority-map]]
   [clojure.string :as str]
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



(comment
  (let [{:keys [graph start targets]} (parse test-input)]
    (utils/dijkstra graph start targets))
  )

(defn part-1 [input]
  (let [{:keys [graph start targets]} (parse input)]
    (-> (utils/dijkstra graph start targets)
        first
        long)))

(comment
  (part-1 test-input)
  ;; => 7036
  (time (part-1 input))
  ;; "Elapsed time: 781.976208 msecs"
  ;; => 105496
  )

(defn dijkstra-alternative
  "returns all shortest paths instead of just 1"
  [graph source targets]
  ;; q : node -> [how many times we've visited it; distance from start node; [previous nodes]]
  (loop [q (priority-map source [0 0 [[]]])
         fuel 1000000]
    (let [[u [^long _ ^double distu paths]] (peek q)]
      (cond
        (zero? fuel) ::out-of-fuel
        ;; we have reached the target
        (targets u)
        [distu (into [] (map #(conj % u)) paths)]
        :else
        (let [new-q
              (reduce
               (fn [q [v ^double weight]]
                 (let [maybe-new-distv (+ distu weight)]
                   (update q v
                           (fn [[_ ^double distv prev-paths :as org]]
                             (if (or
                                  ;; we haven't seen the node yet
                                  (nil? distv)
                                  ;; we have seen it, but this distance is shorter
                                  (< maybe-new-distv distv))
                               ;; then use the new distance
                               [0 maybe-new-distv (into [] (map #(conj % u)) paths)]
                               (if (= maybe-new-distv distv)
                                 [0 maybe-new-distv (into [] (map #(conj % u)) (concat prev-paths paths))]
                                 ;; otherwise do nothing
                                 org))))))
               q
               (graph u))
              ;; mark this node as visited, giving it low priority in the queue,
              new-q (update new-q u update 0 inc)]
          (recur new-q (dec fuel)))))))

(defn part-2 [input]
  (let [{:keys [graph start targets]} (parse input)]
    (count (into #{}
                 (comp cat (map first))
                 (second (dijkstra-alternative graph start targets))))))

(comment
  (part-2 test-input)
  ;; => 45
  (part-2 input)
  ;; => 524
  )

(pop (priority-map :a 1 :b 0))
