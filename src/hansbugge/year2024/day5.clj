(ns hansbugge.year2024.day5
  (:require
   [clojure.string :as str]
   [hansbugge.utils :as utils]))

(defonce input (utils/fetch-input {:year 2024 :day 5}))
(def test-input "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47")

(defn parse [input]
  (let [[rules updates] (->> (str/split-lines input)
                             (split-with (complement #{""}) ))
        updates (->> (rest updates)
                     (map #(->> (str/split % #",")
                                (mapv parse-long))))
        rules (->> rules
                   (map #(->> (str/split % #"\|")
                              (mapv parse-long)))
                   (reduce (fn [acc [n1 n2]] (update acc n1 #(conj (or % #{}) n2))) {}))]
    [rules updates]))

(defn valid? [upd rules]
  (->> upd
       (partition 2 1)
       (every? (fn [[n1 n2]] (not (get-in rules [n2 n1]))))))

(defn middle [upd]
  (nth upd (quot (count upd) 2)))

(defn part-1 [input]
  (let [[rules updates] (parse input)]
    (->> updates
         (filter #(valid? % rules))
         (map middle)
         (apply +))))

(comment
  (part-1 test-input)
  ;; => 143
  (part-1 input)
  ;; => 6612
  )

(defn fix-update [upd rules]
  (sort (fn [n1 n2] (if (get-in rules [n1 n2]) -1 0)) upd))

(defn part-2 [input]
  (let [[rules updates] (parse input)]
    (->> updates
         (remove #(valid? % rules))
         (map #(fix-update % rules))
         (map middle)
         (apply +))))

(comment
  (part-2 test-input)
  ;; => 123
  (part-2 input)
  ;; => 4944
  )
