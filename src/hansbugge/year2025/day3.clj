(ns hansbugge.year2025.day3
  (:require
   [clojure.string :as str]
   [hansbugge.utils :as utils]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defonce input (utils/fetch-input {:year 2025 :day 3}))
(def test-input "987654321111111
811111111111119
234234234234278
818181911112111
")

(defn max-char [cs]
  (char (apply max (map byte cs))))

(defn largest-joltage [s]
  (let [max1 (max-char (drop-last s))
        max2 (max-char (subs s (inc ^long (str/index-of s max1))))]
    (parse-long (str max1 max2))))

(comment
  (largest-joltage "811111111111119")
  (largest-joltage "987654321111111")
  (largest-joltage "234234234234278")
  )

(defn part-1 [input]
  (->> (str/split-lines input)
       (map largest-joltage)
       (apply +)))

(comment
  (part-1 test-input)
  ;; => 357
  (part-1 input)
  ;; => 17301
  )

(defn largest-joltage-2 [s]
  (loop [n 12
         s s
         acc []]
    (if (zero? n)
      (parse-long (apply str acc))
      (let [candidates (take (inc (- (count s) n)) s)
            maxc (max-char candidates)]
        (recur (dec n)
               (subs s (inc ^long (str/index-of s maxc)))
               (conj acc maxc))))))

(defn part-2 [input]
  (->> (str/split-lines input)
       (map largest-joltage-2)
       (apply +)))

(comment
  (part-2 test-input)
  ;; => 3121910778619
  (part-2 input)
  ;; => 172162399742349
  )
