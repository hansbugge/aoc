(ns hansbugge.year2024.day13
  (:require
   [clojure.core.matrix :as matrix]
   [clojure.math :as math]
   [clojure.string :as str]
   [hansbugge.utils :as utils]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(matrix/set-current-implementation :vectorz)

(defonce input (utils/fetch-input {:year 2024 :day 13}))
(def test-input "Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279")

(defn parse [input]
  (->> (str/split input #"\n\n")
       (map utils/numbers)))

(defn check-solution [A b sol]
  (matrix/equals b (matrix/mmul A sol)))

(defn solve-eqs [[a1 a2 b]]
  (let [A (matrix/transpose [a1 a2])]
    (when-not (zero? ^double (matrix/det A))
      (let [double-solution (matrix/mmul (matrix/inverse A) b)
            int-solution (mapv math/round double-solution)]
        (when (check-solution A b int-solution)
          int-solution)))))

(defn price [[^long a ^long b]]
  (+ (* 3 a) b))

(defn part-1 [input]
  (->> (parse input)
       (keep solve-eqs)
       (map price)
       (apply +)))

(comment
  (part-1 test-input)
  ;; => 480
  (part-1 input)
  ;; => 29187
  )

(defn correct [[a1 a2 b]]
  [a1 a2 (mapv #(+ 10000000000000 ^long %) b)])

(defn part-2 [input]
  (->> (parse input)
       (map correct)
       (keep solve-eqs)
       (map price)
       (apply +)))

(comment
  (part-2 test-input)
  ;; => 875318608908
  (part-2 input)
  ;; => 99968222587852
  )
