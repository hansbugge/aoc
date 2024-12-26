(ns hansbugge.year2024.day25
  (:require
   [clojure.math.combinatorics :as combo]
   [clojure.string :as str]
   [hansbugge.utils :as utils]
   [net.cgrand.xforms :as xf]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defonce input (utils/fetch-input {:year 2024 :day 25}))
(def test-input "#####\n.####\n.####\n.####\n.#.#.\n.#...\n.....\n\n#####\n##.##\n.#.##\n...##\n...#.\n...#.\n.....\n\n.....\n#....\n#....\n#...#\n#.#.#\n#.###\n#####\n\n.....\n.....\n#.#..\n###..\n###.#\n###.#\n#####\n\n.....\n.....\n.....\n#....\n#.#..\n#.#.#\n#####")

(defn parse [input]
  (->> (for [lock-or-key (str/split input #"\n\n")
             :let [transposed (apply map vector (utils/grid lock-or-key))
                   signature (map #(dec ^long (xf/count (filter #{\#}) %)) transposed)
                   tipe (if (= \. (first lock-or-key)) :keys :locks)]]
         {tipe [signature]})
       (apply merge-with concat)))

(defn overlaps? [[lock key]]
  (every? #(<= % 5) (map + lock key)))

(defn part-1 [input]
  (->> (parse input)
       vals
       (apply combo/cartesian-product)
       (xf/count (filter overlaps?))))

(comment
  (part-1 test-input)
  ;; => 3
  (part-1 input)
  ;; => 3608
  )
