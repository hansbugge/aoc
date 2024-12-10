(ns hansbugge.year2024.day10
  (:require
   [hansbugge.utils :as utils]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defonce input (utils/fetch-input {:year 2024 :day 10}))
(def test-input "89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732")

(defn trailhead-score
  ([g p]
   (let [visit (fn visit [p ^long n]
                 (when (= n (get-in g p))
                   (if (= 9 n)
                     #{p}
                     (into #{}
                           (mapcat #(visit (utils/step p %) (inc n)))
                           [:n :e :s :w]))))]
     (count (visit p 0)))))

(trailhead-score (utils/grid test-input (comp parse-long str)) [0 2])
;; => 5

(defn part-1 [input]
  (let [g (utils/grid input (comp parse-long str))]
    (->> (utils/points g)
         (map #(trailhead-score g %))
         (apply +))))

(comment
  (part-1 test-input)
  ;; => 36
  (part-1 input)
  ;; => 667
  )

(defn trailhead-rating [g p]
  (let [visit (fn visit [p path ^long n]
                ;; #dbg
                (when (= n (get-in g p))
                  (if (= 9 n)
                    [(conj path p)]
                    (into #{}
                          (mapcat #(visit (utils/step p %) (conj path p) (inc n)))
                          [:n :e :s :w]))))]
    (count (visit p [] 0))))

(defn part-2 [input]
  (let [g (utils/grid input (comp parse-long str))]
    (->> (utils/points g)
         (map #(trailhead-rating g %))
         (apply +))))

(comment
  (part-2 test-input)
  ;; => 81
  (part-2 input)
  ;; => 1344
  )
