(ns hansbugge.year2025.day5
  (:require
   [clojure.string :as str]
   [hansbugge.utils :as utils]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defonce input (utils/fetch-input {:year 2025 :day 5}))
(def test-input "3-5
10-14
16-20
12-18

1
5
8
11
17
32
")

(defn parse [input]
  (let [[ranges ingredients] (str/split input #"\n\n")
        ranges (->> (str/split-lines ranges)
                    (mapv #(->> (str/split % #"-")
                                (mapv parse-long))))
        ingredients (->> (str/split-lines ingredients)
                         (map parse-long))]
    {:ranges ranges
     :ingredients ingredients}))

(defn in-range? [ingredient [a b]]
  (<= a ingredient b))

(defn fresh-ingredients [ranges ingredients]
  (sequence (filter (fn [ingredient]
                      (some #(in-range? ingredient %) ranges)))
            ingredients))

(defn part-1 [input]
  (let [{:keys [ranges ingredients]} (parse input)]
    (count (fresh-ingredients ranges ingredients))))

(comment
  (part-1 test-input)
  ;; => 3
  (part-1 input)
  ;; => 613
  )

(defn split
  "Make ranges rs such that rs and r2 are non-overlapping, and cover the
  same set as ranges r1,r2."
  [r1 r2]
  (let [[^long a ^long b] r1
        [^long c ^long d] r2]
    (cond
      (<= c a d)
      (if (< d b)
        ;;    ----
        ;; -----
        [[(inc d) b]]
        ;;    ----
        ;; ---------
        [])

      (<= c b d)
      ;; ------
      ;;   -------
      [[a (dec c)]]

      (<= a c b)
      ;; ----------
      ;;   -----
      (filterv (fn [[^long x ^long y]] (<= x y))
               [[a (dec c)]
                [(inc d) b]])

      :else
      [[a b]])))

(defn non-overlapping
  "Make ranges qs such that qs and rs are disjoint while covering the same set."
  [r rs]
  (let [[r2 & rs] rs]
    (if (nil? r2)
      [r]
      (mapcat #(non-overlapping % rs) (split r r2)))))

(defn disjoint-ranges [ranges]
  (->> (iterate rest ranges)
       (take-while seq)
       (mapcat (fn [[r1 & rs]] (non-overlapping r1 rs)))))

(comment
  (disjoint-ranges [[0 25] [17 18] [3 5] [10 14] [16 20] [12 18]])
  ;; => [[0 2] [6 9] [21 25] [3 5] [10 11] [19 20] [12 18]]
  )

(defn part-2 [input]
  (let [{:keys [ranges]} (parse input)]
    (->> (disjoint-ranges ranges)
         (map (fn [[^long a ^long b]] (inc (- b a))))
         (apply +))))

(comment
  (part-2 test-input)
  ;; => 14
  (part-2 input)
  ;; => 336495597913098
  )
