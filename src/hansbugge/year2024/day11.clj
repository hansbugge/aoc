(ns hansbugge.year2024.day11
  (:require
   [clojure.math :as math]
   [hansbugge.utils :as utils]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defonce input (utils/fetch-input {:year 2024 :day 11}))

(defn parse [input]
  (-> (utils/numbers input)
      first
      vec))

(defn split [^long n]
  (let [digits (-> (math/log10 n) long inc (quot 2))
        divisor (long (math/pow 10 digits))]
    [(quot n divisor)
     (rem  n divisor)]))

(comment
  (split 1234) ;; => [12 34]
  (split 990001) ;; => [990 1]
  )

(defn even-number-of-digits? [n]
  (-> n math/log10 long odd?))

(defn rule [^long n]
  (cond
    (zero? n) [1]
    (even-number-of-digits? n) (split n)
    :else [(* 2024 n)]))

(defn rules [ns]
  (into [] (mapcat rule) ns))

(def count-stones
  (memoize
   (fn [ns ^long iters]
     (if (zero? iters)
       (count ns)
       (->> (rules ns)
            (mapv #(count-stones [%] (dec iters)))
            (apply +))))))

(defn part-1 [input]
  (count-stones (parse input) 25))

(comment
  (part-1 "125 17")
  ;; => 55312
  (time (part-1 input))
  ;; "Elapsed time: 6.168792 msecs"
  ;; => 194782
  )

(defn part-2 [input]
  (count-stones (parse input) 75))

(comment
  (part-2 "125 17")
  ;; => 65601038650482

  (time (part-2 input))
  ;; "Elapsed time: 139.692125 msecs"
  ;; => 233007586663131

  )
