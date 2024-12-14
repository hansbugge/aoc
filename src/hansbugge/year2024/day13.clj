(ns hansbugge.year2024.day13
  (:require
   [clojure.core.matrix :as matrix]
   [clojure.math :as math]
   [clojure.string :as str]
   [hansbugge.utils :as utils]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;;; (time (dotimes [_ 1000] (part-2 input))) with different matrix representations
(matrix/set-current-implementation :vectorz)
;; "Elapsed time: 947.477042 msecs"
;; (matrix/set-current-implementation :persistent-vector)
;; "Elapsed time: 1820.573625 msecs"
;; (matrix/set-current-implementation :ndarray)
;; "Elapsed time: 4599.744583 msecs"
;; (matrix/set-current-implementation :persistent-map)
;; "Elapsed time: 10825.201708 msecs"
;; (matrix/set-current-implementation :double-array)
;; "Elapsed time: 75876.271834 msecs"

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
       (mapv (fn [l]
               (let [[a1 a2 b] (utils/numbers l)]
                 [(matrix/transpose (matrix/matrix [a1 a2]))
                  (matrix/array b)])))))

(defn check-solution [A b sol]
  (matrix/equals (matrix/mmul A sol) b))


(defn solve-eqs [[A b]]
  (when-not (zero? ^double (matrix/det A))
    (let [double-solution (matrix/mmul (matrix/inverse A) b)
          int-solution (mapv math/round double-solution)]
      (when (check-solution A b int-solution)
        int-solution))))

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

(defn correct [[A b]]
  [A (matrix/add b [1E13 1E13])])

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
