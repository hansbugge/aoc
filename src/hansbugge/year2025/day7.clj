(ns hansbugge.year2025.day7
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [hansbugge.utils :as utils]
   [medley.core :as m]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defonce input (utils/fetch-input {:year 2025 :day 7}))
(def test-input ".......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............
")

;; naive, blows the stack
(defn splitters-naive
  "return coordinates of splitters hit in grid g from start position pos"
  ([g pos] (splitters-naive g pos #{} 1000))
  ([g pos seen ^long fuel]
   (when (<= fuel 0) (throw (ex-info "out of fuel" {})))
   (if-some [v (get-in g pos)]
     (if (not= \^ v)
       (splitters-naive g (utils/step pos :s) seen (dec fuel))
       (if (seen pos)
         #{}
         (let [seen (conj seen pos)
               left (splitters-naive g (utils/step pos :w) seen (dec fuel))
               right (splitters-naive g (utils/step pos :e) seen (dec fuel))]
           (-> (set/union left right)
               (conj pos)))))
     #{})))

;; explicit stack
(defn splitters
  [g pos]
  (loop [stack [pos]
         seen #{}]
    (if-some [pos (peek stack)]
      (let [v (get-in g pos)]
        (cond
          (or (nil? v) (seen pos))
          (recur (pop stack) seen)

          (= \^ v)
          (recur (-> (pop stack)
                     (conj (utils/step pos :w)
                           (utils/step pos :e)))
                 (conj seen pos))

          :else
          (recur (-> (pop stack) (conj (utils/step pos :s))) seen)))
      seen)))

(defn part-1 [input]
  (let [g (utils/grid input)
        ;; S is on first row
        start-pos [0 (let [top (first g)] (m/find-first (comp #{\S} top) (range (count top))))]]
    (count (splitters g start-pos))))

(comment
  (part-1 test-input)
  ;; => 21
  (part-1 input)
  ;; => 1553
  )

(def count-beams
  (memoize
   (fn ^long [g pos]
     (let [v (get-in g pos)]
       (cond
         (nil? v)
         1

         (= \^ v)
         (+ ^long (count-beams g (utils/step pos :w))
            ^long (count-beams g (utils/step pos :e)))

         :else
         (count-beams g (utils/step pos :s)))))) )

(defn part-2 [input]
  (let [g (utils/grid input)
        ;; S is on first row
        start-pos [0 (let [top (first g)] (m/find-first (comp #{\S} top) (range (count top))))]]
    (count-beams g start-pos)))

(comment
  (part-2 test-input)
  ;; => 40
  (part-2 input)
  ;; => 15811946526915
  )
