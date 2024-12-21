(ns hansbugge.year2024.day21
  (:require
   [clojure.string :as str]
   [hansbugge.utils :as utils]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defonce input (utils/fetch-input {:year 2024 :day 21}))
(def test-input "029A\n980A\n179A\n456A\n379A")

;; +---+---+---+
;; | 7 | 8 | 9 |
;; +---+---+---+
;; | 4 | 5 | 6 |
;; +---+---+---+
;; | 1 | 2 | 3 |
;; +---+---+---+
;;     | 0 | A |
;;     +---+---+

(def numeric-keypad-graph
  {\7 #{{:to \4 :label \v} {:to \8 :label \>}}
   \8 #{{:to \7 :label \<} {:to \5 :label \v} {:to \9 :label \>}}
   \9 #{{:to \8 :label \<} {:to \6 :label \v}}
   \4 #{{:to \7 :label \^} {:to \5 :label \>} {:to \1 :label \v}}
   \5 #{{:to \4 :label \<} {:to \8 :label \^} {:to \6 :label \>} {:to \2 :label \v}}
   \6 #{{:to \9 :label \^} {:to \5 :label \<} {:to \3 :label \v}}
   \1 #{{:to \4 :label \^} {:to \2 :label \>}}
   \2 #{{:to \1 :label \<} {:to \5 :label \^} {:to \3 :label \>} {:to \0 :label \v}}
   \3 #{{:to \2 :label \<} {:to \6 :label \^} {:to \A :label \v}}
   \0 #{{:to \2 :label \^} {:to \A :label \>}}
   \A #{{:to \0 :label \<} {:to \3 :label \^}}})

;;     +---+---+
;;     | ^ | A |
;; +---+---+---+
;; | < | v | > |
;; +---+---+---+

(def directional-keypad-graph
  {\^ #{{:to \A :label \>} {:to \v :label \v}}
   \A #{{:to \^ :label \<} {:to \> :label \v}}
   \< #{{:to \v :label \>}}
   \v #{{:to \< :label \<} {:to \^ :label \^} {:to \> :label \>}}
   \> #{{:to \v :label \<} {:to \A :label \^}}})

(defn shortest-path-candidates [instructions graph]
  (let [shortest
        (fn [from to]
          (let [[_ paths] (utils/dijkstra* graph from #{to})]
            (map #(conj (mapv :label %) \A) paths)))]
    (->> (cons \A instructions)
         (partition 2 1)
         (map (fn [[from to]] (shortest from to)))
         (map #(mapv (fn [path] (apply str path)) %)))))

(def length
  (memoize (fn
             ([n instrs]
              (length n n instrs))
             ([start-at ^long n instrs]
              (if (zero? n)
                (count instrs)
                (let [alternatives (shortest-path-candidates
                                    instrs
                                    (if (= start-at n)
                                      numeric-keypad-graph
                                      directional-keypad-graph))]
                  (->> alternatives
                       (map (fn [alts] (apply min (map #(length start-at (dec n) %) alts))))
                       (apply +))))))))

(defn code->score [n code]
  (*
   ^long (length n code)
   (long (parse-long (subs code 0 3)))))

(defn part-1 [input]
  (->> (str/split-lines input)
       (map #(code->score 3 %))
       (apply +)))

(comment
  (part-1 test-input)
  ;; => 126384

  (part-1 input)
  ;; => 171596
  )

(defn part-2 [input]
  (->> (str/split-lines input)
       (map #(code->score 26 %))
       (apply +)))

(comment
  (time (part-2 input))
  ;; => 209268004868246
  )
