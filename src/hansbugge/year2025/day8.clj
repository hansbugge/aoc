(ns hansbugge.year2025.day8
  (:require
   [clojure.math.combinatorics :as combo]
   [hansbugge.utils :as utils]
   [engelberg.data.union-find :as uf]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defonce input (utils/fetch-input {:year 2025 :day 8}))
(def test-input "162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689
")

(defn dist-square [[^long x1 ^long y1 ^long z1] [^long x2 ^long y2 ^long z2]]
  (let [sq (fn [^long x] (* x x))]
    (+ ^long (sq (- x2 x1))
       ^long (sq (- y2 y1))
       ^long (sq (- z2 z1)))))

(defn part-1 [input n]
  (let [points (->> (utils/numbers input)
                    (map vec))
        closest (->> (combo/combinations points 2)
                     (map (fn [[j1 j2]] [(dist-square j1 j2) j1 j2]))
                     sort
                     (take n))
        uf (apply uf/union-find points)
        uf (reduce (fn [acc [_ j1 j2]] (uf/connect acc j1 j2)) uf closest)]
    (->> (uf/components uf)
         (map count)
         (sort #(compare %2 %1))
         (take 3)
         (apply *))))

(comment
  (part-1 test-input 10)
  ;; => 40
  (part-1 input 1000)
  ;; => 81536
  )

(defn part-2 [input]
  (let [points (->> (utils/numbers input)
                    (map vec))
        sorted (->> (combo/combinations points 2)
                    (map (fn [[j1 j2]] [(dist-square j1 j2) j1 j2]))
                    sort)
        uf (apply uf/union-find points)]
    (reduce (fn [uf [_ j1 j2]]
              (let [new-uf (uf/connect uf j1 j2)]
                (if (= 1 (uf/count-components new-uf))
                  (reduced (* (first j1) (first j2)))
                  new-uf))) uf sorted)))

(comment
  (part-2 test-input)
  ;; => 25272
  (part-2 input)
  ;; => 7017750530
  )
