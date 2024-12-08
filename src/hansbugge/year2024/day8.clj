(ns hansbugge.year2024.day8
  (:require
   [hansbugge.utils :as utils]
   [clojure.math.combinatorics :as combo]
   [net.cgrand.xforms :as xf]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defonce input (utils/fetch-input {:year 2024 :day 8}))
(def test-input "............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............")

(defn antennas [g]
  (reduce (fn [m p]
            (let [x (get-in g p)]
              (if (#{\. nil} x)
                m
                (update m x (fnil conj #{}) p))))
          {}
          (utils/points g)))

(defn part-1 [input]
  (let [g (utils/grid input)]
    (xf/count
     (comp (mapcat #(combo/combinations (val %) 2))
           (mapcat (fn [[p1 p2]]
                     (let [diff (map - p1 p2)]
                       [(map + p1 diff) (map - p2 diff)])))
           (filter #(utils/within-grid? g %))
           (distinct))
     (antennas g))))

(comment
  (part-1 test-input)
  ;; => 14
  (part-1 input)
  ;; => 214
  )

(defn part-2 [input]

  (let [g (utils/grid input)]
    (xf/count
     (comp (mapcat #(combo/combinations (val %) 2))
           (mapcat (fn [[p1 p2]]
                     (let [diff (map - p1 p2)]
                       (concat
                        (eduction (comp
                                   (map (fn [^long idx]
                                          (map + p1 (map #(* idx ^long %) diff))))
                                   (take-while #(utils/within-grid? g %)))
                                  (range))
                        (eduction (comp
                                   (map (fn [^long idx]
                                          (map - p2 (map #(* idx ^long %) diff))))
                                   (take-while #(utils/within-grid? g %)))
                                  (range))))))
           (distinct))
     (antennas g))))


(comment
  (part-2 test-input)
  ;; => 34
  (part-2 input)
  ;; => 809
  )
