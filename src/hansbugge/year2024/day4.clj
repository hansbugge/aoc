(ns hansbugge.year2024.day4
  (:require
   [clojure.string :as str]
   [hansbugge.utils :as utils]
   [net.cgrand.xforms :as xf]))

(defonce input (utils/fetch-input {:year 2024 :day 4}))
(def test-input "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX")

(defn word-at-point [w word point direction]
  (if (seq word)
    (let [c (get-in w point)]
      (if (and (some? c)
               (= c (first word)))
        (word-at-point w (rest word) (utils/step point direction) direction)
        0))
    1))

(word-at-point (utils/grid test-input) "XMAS" [0 5] :e)
;; => 1

(defn words-at-point [w word [x y]]
  (apply + (map #(word-at-point w word [x y] %) utils/all-directions)))

(words-at-point (utils/grid test-input) "XMAS" [0 4])
;; => 1

(defn part-1 [input]
  (let [w (utils/grid input)]
    (->> (utils/points w)
         (map #(words-at-point w "XMAS" %))
         (apply +))))

(comment
  (part-1 test-input)
  ;; => 18
  (part-1 input)
  ;; => 2462
  )

(defn x-mas? [w [x y]]
  (and
   ;; the point should be the middle, so an A
   (= \A (get-in w [x y]))
   ;; get the diagonals around the A and check whether they are MS or SM
   (let [f (fn [dirs] (apply str (map #(get-in w (utils/step [x y] %)) dirs)))
         word1 (f [:nw :se])
         word2 (f [:sw :ne])]
     (boolean (and (#{"MS" "SM"} word1)
                   (#{"MS" "SM"} word2))))))

(x-mas? (utils/grid test-input) [1 2])
;; => true

(defn part-2 [input]
  (let [w (utils/grid input)]
    (xf/count
     (filter #(x-mas? w %))
     (utils/points w))))

(comment
  (part-2 test-input)
  ;; => 9
  (part-2 input)
  ;; => 1877
  )
