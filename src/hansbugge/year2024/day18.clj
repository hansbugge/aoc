(ns hansbugge.year2024.day18
  (:require
   [clojure.string :as str]
   [hansbugge.utils :as utils]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defonce input (utils/fetch-input {:year 2024 :day 18}))
(def test-input "5,4\n4,2\n4,5\n3,0\n2,1\n6,3\n2,4\n1,5\n0,6\n3,3\n2,6\n5,1\n1,2\n5,5\n2,5\n6,5\n1,4\n0,4\n6,4\n1,1\n6,1\n1,0\n0,5\n1,6\n2,0")

(defn byte-positions [input]
  (map vec (utils/numbers input)))

(defn graph [bytes steps ^long height ^long width]
  (let [grid (->>
              bytes
              (take steps)
              (reduce
               (fn [g p]
                 (assoc-in g p :byte))
               (utils/sparse-grid height width)))
        neighbors (fn [p]
                    (into #{}
                          (keep (fn [dir]
                                  (let [neighbor (utils/step p dir)]
                                    (when (and (utils/within-grid? grid neighbor)
                                               (nil? (get-in grid neighbor)))
                                      [neighbor 1]))))
                          [:n :s :e :w])) ]
    (into {}
          (comp (filter #(nil? (get-in grid %)))
                (map (fn [p] [p (neighbors p)])))
          (utils/points grid))))

(defn part-1 [input steps ^long height ^long width]
  (-> (graph (byte-positions input) steps height width)
      (utils/dijkstra [0 0] #{[(dec height) (dec width)]})
      first
      long))

(comment
  (part-1 test-input 12 7 7)
  ;; => 22
  (part-1 input 1024 71 71)
  ;; => 262
  )

(defn part-2 [input ^long height ^long width]
  (let [bytes (vec (byte-positions input))]
    (loop [n 1]
      (let [s (-> (graph bytes n height width)
                  (utils/dijkstra [0 0] #{[(dec height) (dec width)]}))]
        (if (= ::utils/out-of-fuel s)
          (str/join "," (bytes (dec n)))
          (recur (inc n)))))))

(comment
  (part-2 test-input 7 7)
  ;; => "6,1"
  (time (part-2 input 71 71))
  ;; "Elapsed time: 52748.619583 msecs"
  ;; => "22,20"
  ;; there's probably a faster and smarter solution, but ¯\_(ツ)_/¯
  )
