(ns hansbugge.year2025.day4
  (:require
   [clojure.string :as str]
   [hansbugge.utils :as utils]
   [net.cgrand.xforms :as xf]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defonce input (utils/fetch-input {:year 2025 :day 4}))
(def test-input (str/trim "
..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.
"))

(defn neighbours [grid p]
  (xf/count
   (filter #(= \@ (get-in grid (utils/step p %))))
   utils/all-directions))

(comment
  (neighbours (utils/grid test-input) [1 0])
  )

(defn part-1 [input]
  (let [grid (utils/grid input)
        points (utils/points grid)]

    (xf/count (comp (filter #(= \@ (get-in grid %)))
                    (filter #(< (neighbours grid %) 4)))
              points)))

(comment
  (part-1 test-input)
  ;; => 13
  (part-1 input)
  ;; => 1523
  )

(defn part-2 [input]
  (let [init-grid (utils/grid input)
        points (utils/points init-grid)]
    (loop [[grid prev-grid] [init-grid nil]
           removed #{}]
      (if (= prev-grid grid)
        (count removed)
        (let [removable (into #{}
                              (comp (filter #(= \@ (get-in grid %)))
                                    (filter #(< (neighbours grid %) 4)))
                              points)
              new-grid (reduce (fn [acc p] (assoc-in acc p \.))
                               grid removable)]
          (recur [new-grid grid] (into removed removable))))))

  )

(comment
  (part-2 test-input)
  ;; => 43
  (part-2 input)
  ;; => 9290
  )
