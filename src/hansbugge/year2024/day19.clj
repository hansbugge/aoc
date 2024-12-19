(ns hansbugge.year2024.day19
  (:require
   [clojure.string :as str]
   [hansbugge.utils :as utils]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defonce input (utils/fetch-input {:year 2024 :day 19}))
(def test-input "r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb")

(defn parse [input]
  (let [[towels designs] (str/split input #"\n\n")]
    {:towels (str/split towels #"\, ")
     :designs (str/split-lines designs)}))

(def combo-count
  (memoize
   (fn [towels design]
     (if (= "" design)
       1
       (->> towels
            (filter #(str/starts-with? design %))
            (map #(combo-count towels (subs design (count %))))
            (apply +))))))

(defn part-1 [input]
  (let [{:keys [towels designs]} (parse input)]
    (count (remove #(zero? ^long (combo-count towels %)) designs))))

(comment
  (part-1 test-input)
  ;; => 6
  (part-1 input)
  ;; => 342
  )

(defn part-2 [input]
  (let [{:keys [towels designs]} (parse input)]
    (transduce (map #(combo-count towels %)) + designs)))

(comment
  (part-2 test-input)
  ;; => 16
  (time (part-2 input))
  ;; => 891192814474630
  )
