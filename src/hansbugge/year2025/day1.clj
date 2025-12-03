(ns hansbugge.year2025.day1
  (:require
   [clojure.string :as str]
   [hansbugge.utils :as utils]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defonce input (utils/fetch-input {:year 2025 :day 1}))
(def test-input "L68
L30
R48
L5
R60
L55
L1
L99
R14
L82
")

(defn part-1 [input]
  (->> (str/split-lines input)
       (map (juxt first (comp parse-long #(subs % 1))))
       (reduce (fn [{:keys [value] :as acc} [dir v]]
                 (let [^long new-value (mod ((if (= \L dir) - +) value v) 100)]
                   (-> acc
                       (assoc :value new-value)
                       (cond-> (zero? new-value)
                         (update :zero-count inc)))))
               {:value 50
                :zero-count 0})
       :zero-count))

(comment
  (part-1 test-input)
  ;; => 3
  (part-1 input)
  ;; => 1055
  )

(defn step [{:keys [^long value] :as acc} [dir v]]
  (let [^long new-raw-value ((if (= \L dir) - +) value v)
        new-value (mod new-raw-value 100)]
    (-> acc
        (assoc :value new-value)
        (update :zero-count
                (fnil + 0)
                (quot (if (pos? new-raw-value)
                        new-raw-value
                        (+ 100 (- new-raw-value)))
                      100))
        (cond-> (and (zero? value) (= \L dir))
          (update :zero-count dec)))))

(defn part-2 [input]
  (->> (str/split-lines input)
       (map (juxt first (comp parse-long #(subs % 1))))
       (reduce step {:value 50 :zero-count 0})
       :zero-count))

(comment
  (part-2 test-input)
  ;; => 6
  (part-2 input)
  ;; => 6386
  )
