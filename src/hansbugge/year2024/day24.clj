(ns hansbugge.year2024.day24
  (:require
   [clojure.pprint :as pprint]
   [clojure.string :as str]
   [hansbugge.utils :as utils]
   [ubergraph.alg :as ua]
   [ubergraph.core :as ug]
   [medley.core :as m]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defonce input (utils/fetch-input {:year 2024 :day 24}))

(defn parse [input]
  (let [[init-vals gates] (str/split input #"\n\n")]
    {:init-vals (into {} (comp (map #(str/split % #": "))
                               (map (juxt first (comp parse-long second))))
                      (str/split-lines init-vals))
     :gates (->> (str/split-lines gates)
                 (map #(let [[_ v1 o v2 r] (re-matches #"(.{3}) (AND|OR|XOR) (.{3}) -> (.{3})" %)]
                         (into [(keyword (str/lower-case o))] [v1 v2 r])))
                 (into []))}))

(defn ->graph [input]
  (let [{:keys [init-vals gates]} (parse input)
        edges (into (for [[wire v] init-vals]
                      [[:init v]
                       wire])
                    cat
                    (for [[_ w1 w2 w3 :as n] gates]
                      [[w1 n]
                       [w2 n]
                       [n w3]]))]
    (apply ug/digraph edges)))

(defn simulate [g]
  (when-let [s (ua/topsort g)]
    (reduce
     (fn [g node]
       (cond
         (string? node)
         ;; wire, carry the value over from predecessor
         (let [pred (first (ug/predecessors g node))
               _ (assert pred "there should be one predecessor")]
           (ug/add-attr g node :val (ug/attr g pred :val)))

         ;; init-node, set val to literal value
         (= :init (first node))
         (ug/add-attr g node :val (second node))

         :else
         ;; gate, there should be two predecessors
         (let [preds (ug/predecessors g node)
               _ (assert (= 2 (count preds)) "there should be exactly two predecessors")
               op (case (first node)
                    :and bit-and
                    :or bit-or
                    :xor bit-xor)
               input (map #(ug/attr g % :val) preds)]
           (ug/add-attr g node :val (apply op input)))))
     g
     s)))

(defn get-binary-val [g var-name]
  (->> g
       :attrs
       (filter #(str/starts-with? (key %) var-name))
       (sort #(compare %2 %1))
       (map (comp :val val))
       (apply str)))

(defn to-binary ^String [^long n]
  (Long/toBinaryString n))

(defn from-binary ^long [^String b]
  (BigInteger. b 2))

(defn part-1 [input]
  (-> (->graph input)
      simulate
      (get-binary-val "z")
      (from-binary)))

(comment
  (time (part-1 input))
  ;; => 46362252142374
  )

(comment
  ;;; Part 2: figure out which wires to swap.

  ;;; Desired:
  (let [g (simulate (->graph input))]
    (-> (+ (from-binary (get-binary-val g "x"))
           (from-binary (get-binary-val g "y")))
        to-binary))
  ;; => "1010011110101010001101100100100000011011100110"

  ;;; Current:
  (-> (->graph input)
      simulate
      (get-binary-val "z"))
  ;; => "1010100010101010001101100100011110011100100110"


  ;;; bits to change:
  ;;; z06..z08 should be flipped (100 -> 011)
  ;;; z13..z17 should be flipped (01111 -> 10000)
  ;;; z38..z41 should be flipped (1000 -> 0111)

  ;;; current vs. desired:
  ;;;"1010 1000 10101010001101100100 01111 0011 100 100110"
  ;;;"1010 0111 10101010001101100100 10000 0011 011 100110"
  )

(defn swap-output-wires [g w1 w2]
  (let [nodes (ug/nodes g)
        n1 (m/find-first #(= w1 (nth % 3 nil)) nodes)
        n2 (m/find-first #(= w2 (nth % 3 nil)) nodes)]
    (-> g
        (ug/remove-edges [n1 w1]
                         [n2 w2])
        (ug/add-edges [n1 w2]
                      [n2 w1]))))


(def g (->graph input))

(comment
  (ug/viz-graph g {:save {:filename "graph.pdf" :format :pdf}})
  )

(comment
  ;;; The input seems to be (almost) a ripple-carry adder.
  ;;; It consists of adder components with 5 gates, taking as input
  ;;; x(n), y(n), carry(n) and giving output z(n+1), carry(n+1).

  ;;; Schematics for the adder components:

  ;;                                      +--------------+
  ;;                                      |              |
  ;; carry_in ----------------------+-----+              |
  ;;       +-----------+            |     |      XOR     +-------  z
  ;;       |           |     o------.-----+              |
  ;; x --+-+           |     |      |     |              |
  ;;     | |    XOR    +-mxor+      |     +--------------+
  ;; y -+.-+           |     |      |
  ;;    || |           |     |      |     +--------------+
  ;;    || +-----------+     |      |     |              |
  ;;    ||                   |      +-----+              |
  ;;    ||                   |            |      AND     +mandxor+
  ;;    ||                   +------------+              |       |
  ;;    ||                                |              |       |
  ;;    ||                                +--------------+       |
  ;;    ||                                                       |  +-----------+
  ;;    || +-----------+                                         |  |           |
  ;;    || |           |                                         +--+           |
  ;;    |+-+           |                                            |    OR     +- carry_out
  ;;    |  |    AND    +--mand--------------------------------------+           |
  ;;    +--+           |                                            |           |
  ;;       |           |                                            +-----------+
  ;;       +-----------+

  ;; we should have one of these components for each bit.


  ;;; we build a ripple carry adder validator and manually fix the errors:
  )

(defn validate [g]
  (let [gates (->> (ua/topsort g)
                   (filter vector?)
                   (remove (comp #{:init} first))
                   (map (fn [[op :as node]]
                          (let [preds (set (ug/predecessors g node))
                                output (first (ug/successors g node))]
                            [[op preds] output])))
                   (into {}))]
    (loop [idx 1
           carry "nqp"]
      (if (<= 45 idx)
        ::its-a-good-adder
        (let [fmt #(format "%s%02d" % idx)
              in (into #{} (map fmt) ["x" "y"])
              out (fmt "z")
              mand (or (gates [:and in])
                       (throw (ex-info "no mand" {:idx idx :carry carry})))
              mxor (or (gates [:xor in])
                       (throw (ex-info "no mxor" {:idx idx :carry carry})))
              res (or (gates [:xor #{mxor carry}])
                      (throw (ex-info "no res" {:idx idx :carry carry :mxor mxor :mand mand})))
              _ (when-not (= out res)
                  (throw (ex-info "res wrong" {:idx idx :carry carry :mxor mxor :mand mand :res res
                                               :res-should-be out})))
              mandxor (or (gates [:and #{carry mxor}])
                          (throw (ex-info "no mandxor" {:idx idx :carry carry :mxor mxor})))
              carryout (gates [:or #{mandxor mand}])]
          (recur (inc idx) carryout))))))

(comment
  (validate g)
  ;; iterate through the errors and fix them manually:

  (-> g
      (swap-output-wires "z06" "jmq")
      (swap-output-wires "gmh" "z13")
      (swap-output-wires "rqf" "cbd")
      (swap-output-wires "qrh" "z38")
      validate)
  ;; => :hansbugge.year2024.day24/its-a-good-adder

  (->> ["z06" "jmq"
        "gmh" "z13"
        "rqf" "cbd"
        "qrh" "z38"]
       sort
       (str/join ","))
  ;; => "cbd,gmh,jmq,qrh,rqf,z06,z13,z38"
  )
