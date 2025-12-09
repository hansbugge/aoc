(ns hansbugge.year2025.day6
  (:require
   [clojure.string :as str]
   [hansbugge.utils :as utils]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defonce input (utils/fetch-input {:year 2025 :day 6}))
(def test-input
  (str
   "123 328  51 64 " \newline
   " 45 64  387 23 " \newline
   "  6 98  215 314" \newline
   "*   +   *   +  " \newline))

(defn part-1 [input]
  (let [lines (str/split-lines input)
        numbers (pop lines)
        ops (peek lines)
        number-cols (->> numbers
                         (mapv #(mapv parse-long (re-seq #"-?\d+" %)))
                         utils/transpose)
        ops (->> (str/split ops #"\s+")
                 (mapv {"*" *, "+" +}))]
    (apply + (map apply ops number-cols))))

(comment
  (part-1 test-input)
  ;; => 4277556
  (part-1 input)
  ;; => 6417439773370
  )

(defn parse-blocks [input]
  (let [lines (str/split-lines input)
        numbers (pop lines)
        ops (-> (peek lines)
                (str/split #" (?=[\+\*])"))]
    (loop [acc (mapv (comp vector {\* *, \+ +} first) ops)
           [row & rows] numbers]
      (if (nil? row)
        acc
        (recur
         (loop [i 0
                idx 0
                acc acc]
           (if (<= (count ops) i)
             acc
             (let [op (ops i)
                   next-idx (+ idx (count op))]
               (recur (inc i)
                      (inc next-idx)
                      (update acc i conj (subs row idx next-idx))))))
         rows)))))

(defn calc
  "calculate a block such as [* \"123\" \" 45\" \"  6\"]"
  [block]
  (let [[op & rect] block]
    (->> rect
         (apply map str)
         (map str/trim)
         (map parse-long)
         (apply op))))

(comment
  (parse-blocks test-input)
  (calc [* "123" " 45" "  6"])
  ;; => 8544
  )

(defn part-2 [input]
  (->> (parse-blocks input)
       (map calc)
       (apply +)))

(comment
  (part-2 test-input)
  ;; => 3263827
  (part-2 input)
  ;; => 11044319475191
  )
