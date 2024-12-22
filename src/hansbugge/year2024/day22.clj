(ns hansbugge.year2024.day22
  (:require
   [hansbugge.utils :as utils]
   [net.cgrand.xforms :as xf]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defonce input (utils/fetch-input {:year 2024 :day 22}))
(def test-input "1\n10\n100\n2024")

(defn parse [input]
  (map first (utils/numbers input)))

(defn prune ^long [^long secret]
  ;; bit-and with 2^24-1, i.e. take the 24 right-most bits
  (bit-and secret 16777215))

(prune 100000000)
;; => 16113920


(defn step-1 ^long [^long n]
  (-> n
      (bit-shift-left 6) ; * 64
      (bit-xor n)
      prune))

(defn step-2 ^long [^long n]
  (-> n
      (bit-shift-right 5) ; / 32
      (bit-xor n)
      prune))

(defn step-3 ^long [^long n]
  (-> n
      (bit-shift-left 11) ; * 2048
      (bit-xor n)
      prune))

(defn step [^long n]
  (-> n step-1 step-2 step-3))

(comment
  (->> (iterate step 123) (take 10))
  ;; => (123 15887950 16495136 527345 704524 1553684 12683156 11100544 12249484 7753432)
  )

(defn part-1 [input]
  (->> (parse input)
       (map #(nth (iterate step %) 2000))
       (apply +)))

(comment
  (part-1 test-input)
  ;; => 37327623
  (time (part-1 input))
  ;; "Elapsed time: 90.311459 msecs"
  ;; => 18261820068
  )

(defn secret->changes-map [secret]
  (transduce
   (comp
    (take 2001)
    (xf/partition 2 1)
    (map (fn [[^long s1 ^long s2]] (let [^long n2 (mod s2 10)]
                                     [n2 (- n2 ^long (mod s1 10))])))
    (xf/partition 4 1)
    (map (fn [xs] [(mapv second xs) (first (nth xs 3))])))
   (completing (fn [acc [changes v]]
                 (cond-> acc
                   (not (contains? acc changes)) (assoc changes v))))
   {}
   (iterate step secret)))

(defn part-2 [input]
  (time (->> (parse input)
             (map secret->changes-map)
             (apply merge-with +)
             (sort-by val #(compare %2 %1))
             first)))

(comment
  (part-2 "1\n2\n3\n2024")
  ;; => [[-2 1 -1 3] 23]
  (time (part-2 input))
  ;; "Elapsed time: 6152.819417 msecs"
  ;; => [[0 0 -1 1] 2044]
  )
