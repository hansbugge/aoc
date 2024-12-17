(ns hansbugge.year2024.day17
  {:clj-kondo/config
   '{:linters {:unresolved-symbol {:exclude [(clojure.core.logic/run*)]}}}}
  (:require
   [clojure.string :as str]
   [hansbugge.utils :as utils]
   [medley.core :as m]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defonce input (utils/fetch-input {:year 2024 :day 17}))
(def test-input "Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0")

(defn parse [input]
  (let [[[A] [B] [C] _ prg] (utils/numbers input)]
    {:A A
     :B B
     :C C
     :program (vec prg)
     :output []
     :instruction 0}))
(parse test-input)
;; => {:A 729, :B 0, :C 0, :program (0 1 5 4 3 0)}

(defn pow-2 ^long [^long n]
  (bit-shift-left 1 n))

(defn combo ^long [state ^long operand]
  (case operand
    (0 1 2 3) operand
    4 (:A state)
    5 (:B state)
    6 (:C state)))

(defn step [{:keys [^long A ^long C program ^long instruction] :as state}]
  (when-let [opcode (get program instruction)]
    (let [operand (program (inc instruction))]
      (case (long opcode)
        ;; adv
        0 (-> state
              (update :A quot (pow-2 (combo state operand)))
              (update :instruction + 2))
        ;; bxl
        1 (-> state
              (update :B bit-xor operand)
              (update :instruction + 2))
        ;; bst
        2 (-> state
              (assoc :B (mod (combo state operand) 8))
              (update :instruction + 2))
        ;; jnz
        3 (update state :instruction
                  #(if (zero? A)
                     (+ ^long % 2)
                     operand))
        ;; bxc
        4 (-> state
              (update :B bit-xor C)
              (update :instruction + 2))
        ;; out
        5 (-> state
              (update :output conj (mod (combo state operand) 8))
              (update :instruction + 2))
        ;; bdv
        6 (-> state
              (assoc :B (quot A (pow-2 (combo state operand))))
              (update :instruction + 2))
        ;; cdv
        7 (-> state
              (assoc :C (quot A (pow-2 (combo state operand))))
              (update :instruction + 2))))))

(defn run
  ([state] (run state nil))
  ([state a]
   (->> (m/assoc-some state :A a)
        (iterate step)
        (take-while some?)
        last
        :output)))

(defn part-1 [input]
  (->> (parse input)
       run
       (str/join ",")))

(comment
  (part-1 test-input)
  ;; => "4,6,3,5,6,3,5,2,1,0"
  (part-1 input)
  ;; => "7,1,5,2,4,0,7,6,1"
  )

(def test-input-2 "Register A: 2024
Register B: 0
Register C: 0

Program: 0,3,5,4,3,0")

(defn part-2
  [input]
  (let [{:keys [program] :as state} (parse input)]
    ;; we approximate the result by finding the input that matches on the suffixes
    ;; of the program, using the observation that the suffix is stable when you
    ;; multiply by 8 (shift 3 bits left)
    (->> (range (dec (count program)) -1 -1) ;; [l-1..0] where l = length(program)
         (reduce
          (fn [approximations from-length]
            (let [program-suffix (subvec program from-length)]
              (->> approximations
                   (map #(bit-shift-left ^long % 3))
                   (mapcat #(range % (+ 8 ^long %)))
                   (filter #(= program-suffix (run state %))))))
          [0])
         (apply min))))

(comment
  (part-2 test-input-2)
  ;; => 117440
  (time (part-2 input))
  ;; "Elapsed time: 8.579667 msecs"
  ;; => 37222273957364
  )
