(ns hansbugge.year2024.day15
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [hansbugge.utils :as utils]
   [medley.core :as m]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defonce input (utils/fetch-input {:year 2024 :day 15}))
(def test-input-1 "##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^")

(defn find-robot [g]
  (m/find-first
   (fn [p] (= \@ (get-in g p)))
   (utils/points g)))

(defn parse [input]
  (let [[m ms] (str/split input #"\n\n")
        m (utils/grid m)]
    {:wh-map m
     :pos (find-robot m)
     :movements (into []
                      (comp cat
                            (map {\^ :n
                                  \> :e
                                  \v :s
                                  \< :w}))
                      (str/split-lines ms))}))

(:pos (parse test-input-1))

(defn move-boxes
  "return new wh-map with a box added at the first available spot in dir,
  or nil if that is not possible (e.g. because of hitting a wall)"
  [wh-map pos dir]
  (loop [new-pos (utils/step pos dir)]
    (case (get-in wh-map new-pos)
      \. (assoc-in wh-map new-pos \O)
      \O (recur (utils/step new-pos dir))
      nil)))

(defn step* [wh-map pos dir]
  (let [new-pos (utils/step pos dir)
        new-pos-val (get-in wh-map new-pos)
        new-wh-map (case new-pos-val
                     \. (-> wh-map
                            (assoc-in pos \.)
                            (assoc-in new-pos \@))
                     \O (some-> (move-boxes wh-map new-pos dir)
                                (assoc-in pos \.)
                                (assoc-in new-pos \@))
                     nil)]
    (when new-wh-map
      {:wh-map new-wh-map
       :pos new-pos})))

(defn step [{:keys [wh-map pos movements] :as state}]
  (when-some [dir (first movements)]
    (if-let [new-state (step* wh-map pos dir)]
      (assoc new-state :movements (rest movements))
      (update state :movements rest))))

(comment
  (count (:movements (parse test-input-1)))

  (let [state (parse test-input-1)]
    (->> (iterate step state)
         ;; (take 10)
         (take-while #(seq (:movements %)))
         (map-indexed #(do
                         (println "Steps:" %1)
                         (println "Next movement:" (first (:movements %2)))
                         (utils/print-grid (:wh-map %2))
                         (prn)))
         doall))
  )

(defn gps-sum [wh-map]
  (->> (utils/points wh-map)
       (filter #(#{\O \[} (get-in wh-map %)))
       (map (fn [[^long x ^long y]] (+ (* 100 x) y)))
       (apply +)))

(defn part-1 [input]
  (->> (iterate step (parse input))
       (take-while #(seq (:movements %)))
       (take 100000) ;; failsafe
       last
       :wh-map
       gps-sum))

(comment
  (part-1 test-input-1)
  ;; => 10092
  (part-1 input)
  ;; => 1413675
  )

(defn parse-2 [in]
  (let [[grid ms] (str/split in #"\n\n")
        grid (utils/grid grid)
        robot (find-robot grid)]
    {:grid (assoc-in grid robot \.)
     :robot robot
     :movements (into []
                      (comp cat
                            (map {\^ :n
                                  \> :e
                                  \v :s
                                  \< :w}))
                      (str/split-lines ms))}))

(defn widen [in]
  (str/escape in {\# "##" \O "[]" \. ".." \@ "@."}))

(defn boxes-to-move [grid dir box]
  (let [moving-vertically? (#{:n :s} dir)
        next-box (utils/step box dir)
        check #(boxes-to-move grid dir %)]
    (conj (case (get-in grid next-box)
            \[ (cond-> (check next-box)
                 moving-vertically? (into (check (utils/step next-box :e))))
            \] (cond-> (check next-box)
                 moving-vertically? (into (check (utils/step next-box :w))))
            \# #{::blocked}
            \. #{})
          box)))

(defn move-boxes-2 [grid boxes dir]
  (let [without-old-boxes (reduce (fn [g box] (assoc-in g box \.)) grid boxes)]
    (reduce (fn [g box] (assoc-in g (utils/step box dir) (get-in grid box)))
            without-old-boxes
            boxes)))

(defn make-move [{:keys [grid robot] :as m} dir]
  (let [boxes (boxes-to-move grid dir robot)]
    (if (boxes ::blocked)
      m
      {:grid (move-boxes-2 grid boxes dir)
       :robot (utils/step robot dir)})))

(defn part-2 [input]
  (let [{:keys [movements] :as m} (parse-2 (widen input))]
    (gps-sum (:grid (reduce make-move m movements)))))

(comment
  (part-2 test-input-1)
  ;; => 9021
  (part-2 input)
  ;; => 1399772
  )
