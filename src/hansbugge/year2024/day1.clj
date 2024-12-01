(ns hansbugge.year2024.day1
  (:require
   [clojure.string :as str]
   [hansbugge.utils :as utils]))

(defonce input (utils/fetch-input {:year 2024 :day 1}))

(def test-input "3   4
4   3
2   5
1   3
3   9
3   3")

(defn parse-lists [input]
  (->> (str/split-lines input)
       (map (fn [line] (map parse-long (re-seq #"\d+" line))))
       (apply map vector)))

(defn part-1 [input]
  (->> (parse-lists input)
       (map sort)
       (apply map vector)
       (map (fn [[n1 n2]] (abs (- n1 n2))))
       (apply +)))


(comment
  (part-1 test-input)
  ;; => 11
  (part-1 input)
  ;; => 1938424
  )

(defn part-2 [input]
  (let [[l1 l2] (parse-lists input)
        freqs (frequencies l2)]
    (->> l1
         (map (fn [n] (* n (freqs n 0))))
         (apply +))))

(comment
  (part-2 test-input)
  ;; => 31
  (part-2 input)
  ;; => 22014209
  )
