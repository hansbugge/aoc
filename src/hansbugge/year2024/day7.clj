(ns hansbugge.year2024.day7
  (:require
   [clojure.math :as math]
   [hansbugge.utils :as utils]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defonce input (utils/fetch-input {:year 2024 :day 7}))
(def test-input "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20")

(defn valid
  [^long n ns ops]
  (let [visit (fn visit [^long acc [^long m & ms]]
                (if (or (nil? m) (< n acc))
                  [acc]
                  (eduction (mapcat #(visit (% acc m) ms)) ops)))]
    (some #{n} (visit (first ns) (rest ns)))))

(defn part-1 [input]
  (->> (utils/numbers input)
       (keep #(valid (first %) (rest %) [+ *]))
       (apply +)))

(comment
  (part-1 test-input)
  ;; => 3749
  (part-1 input)
  ;; => 12553187650171
  )

;; (defn || [m n] (parse-long (str m n))) ; simpler but slower

(defn || [^long m ^long n]
  (let [digits (-> n math/log10 long inc)]
    (+ (* m (long (math/pow 10 digits)))
       n)))

(defn part-2 [input]
  (->> (utils/numbers input)
       utils/partition-ncpus
       (pmap (fn [ms]
               (->> ms
                    (keep #(valid (first %) (rest %) [+ * ||]))
                    (apply +))))
       (apply +)))

(comment
  (part-2 test-input)
  ;; => 11387
  (time (part-2 input))
  ;; "Elapsed time: 226.769375 msecs"
  ;; => 96779702119491
  )
