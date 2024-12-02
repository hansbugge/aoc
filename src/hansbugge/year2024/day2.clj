(ns hansbugge.year2024.day2
  (:require
   [clojure.string :as str]
   [hansbugge.utils :as utils]
   [net.cgrand.xforms :as xf]))

(defonce input (utils/fetch-input {:year 2024 :day 2}))

(def test-input "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9")

(defn parse [input]
  (map (fn [line]
         (->> (str/split line #" ")
              (map parse-long)))
       (str/split-lines input)))

(comment
  (parse test-input)
  )

(partition 2 1 [1 2 3 4 5])
;; => ((1 2) (2 3) (3 4) (4 5))

(defn safe? [l]
  (let [deltas (->> (partition 2 1 l)
                    (map #(apply - %)))]
    (or (every? #(<= -3 % -1) deltas)
        (every? #(<= 1 % 3) deltas))))

(defn part-1 [input]
  (xf/count (filter safe?) (parse input)))

(comment
  (part-1 test-input)
  ;; => 2
  (part-1 input)
  ;; => 585
  )

(defn remove-nth [v i]
  (concat (take i v) (drop (inc i) v)))

(defn remove-levels [l]
  (map (fn [i] (remove-nth l i)) (range (inc (count l)))))

(remove-levels [:a :b :c])
;; => ((:b :c) (:a :c) (:a :b) (:a :b :c))

(defn part-2 [input]
  (xf/count (filter #(some safe? (remove-levels %))) (parse input)))

(comment
  (part-2 test-input)
  ;; => 4
  (part-2 input)
  ;; => 626
  )
