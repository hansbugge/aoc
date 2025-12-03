(ns hansbugge.year2025.day2
  (:require
   [clojure.math :as math]
   [clojure.string :as str]
   [hansbugge.utils :as utils]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defonce input (utils/fetch-input {:year 2025 :day 2}))
(def test-input "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124")

(defn invalid-id? [^long n]
  (let [lngth (inc (long (math/log10 n)))]
    (and (even? lngth)
         (let [mid (long (math/pow 10 (quot lngth 2)))]
           (= (rem n mid)
              (quot n mid))))))

(comment
  (inc (int (math/log10 1188511885)))
  ;; => 10
  (=
   (rem 1188511885 (long (math/pow 10 5)))
   (quot 1188511885 (long (math/pow 10 5))))
  (invalid-id? 1188511885)
  (invalid-id? 12)
  )

(defn parse [input]
  (->> (str/split (str/trim input) #",")
       (map #(str/split % #"-"))
       (map #(mapv parse-long %))))

(defn part-1 [input]
  (->> (parse input)
       (mapcat (fn [[x ^long y]] (range x (inc y))))
       (filter invalid-id?)
       (apply +)))

(comment
  (part-1 test-input)
  ;; => 1227775554
  (part-1 input)
  ;; => 23039913998
  )

(defn divisors [n]
  (into #{}
        (comp (filter #(zero? (mod n %)))
              (mapcat (fn [d] [d (quot n d)])))
        (range 1 (inc (int (math/sqrt n))))))

(defn invalid-id2? [n]
  (let [s (str n)
        len (count s)
        divs (disj (divisors len) len)]
    (some (fn [d]
            (apply =
                   (partition d s)))
          divs)))

(comment
  (invalid-id2? 123123123)
  ;; => true
  )

(defn part-2 [input]
  (->> (parse input)
       (mapcat (fn [[x ^long y]] (range x (inc y))))
       (filter invalid-id2?)
       (apply +)))

(comment
  (part-2 test-input)
  ;; => 4174379265
  (part-2 input)
  ;; => 35950619148
  )
