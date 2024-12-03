(ns hansbugge.year2024.day3
  (:require
   [clojure.string :as str]
   [hansbugge.utils :as utils]))

(defonce input (utils/fetch-input {:year 2024 :day 3}))
(def test-input "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")

(defn part-1 [input]
  (->> (re-seq #"mul\((\d+),(\d+)\)" input)
       (map (fn [[_ n1 n2]] (* (parse-long n1) (parse-long n2))))
       (apply +)))

(part-1 test-input)
;; => 161
(part-1 input)
;; => 174103751

(def test-input-2 "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

(defn part-2 [input]
  (->> (str/split input #"do\(\)")
       (map #(first (str/split % #"don't\(\)")))
       (map part-1)
       (apply +)))

(part-2 test-input-2)
;; => 48
(part-2 input)
;; => 100411201
