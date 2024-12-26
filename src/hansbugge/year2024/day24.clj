(ns hansbugge.year2024.day24
  (:require
   [clojure.string :as str]
   [hansbugge.utils :as utils]
   [ubergraph.alg :as ua]
   [ubergraph.core :as ug]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defonce input (utils/fetch-input {:year 2024 :day 24}))
(def test-input "x00: 1\nx01: 1\nx02: 1\ny00: 0\ny01: 1\ny02: 0\n\nx00 AND y00 -> z00\nx01 XOR y01 -> z01\nx02 OR y02 -> z02")

(defn parse [input]
  (let [[assignments constraints] (str/split input #"\n\n")]
    {:assignments (into {} (comp (map #(str/split % #": "))
                                 (map (juxt (comp symbol first) (comp parse-long second))))
                        (str/split-lines assignments))
     :constraints (->> (str/split-lines constraints)
                       (map #(let [[_ v1 o v2 r] (re-matches #"(.{3}) (AND|OR|XOR) (.{3}) -> (.{3})" %)]
                               (into [(keyword (str/lower-case o))] (map symbol) [v1 v2 r])))
                       (into []))}))

;; build graph
(defn ->graph [input]
  (let [{:keys [assignments constraints]} (parse input)
        g (apply ug/digraph
                 [:hi {:val 1}]
                 [:lo {:val 0}]
                 constraints)
        edges (into (for [[wire v] assignments]
                      [(if (= 1 v) :hi :lo) wire])
                    cat
                    (for [[_ w1 w2 w3 :as n] constraints]
                      [[w1 n]
                       [w2 n]
                       [n w3]]))]
    (apply ug/add-directed-edges g edges)))

(comment
  (-> (->graph test-input)
      (ug/viz-graph {:auto-label true}))

  (def *g (->graph input))
  *g
  (ug/viz-graph *g)
  (-> (->graph input)
      (ug/viz-graph {:auto-label false
                     :save {:filename "graph.png" :format :png}}))
  )

(defn simulate [g]
  (reduce
   (fn [g node]
     (cond
       (keyword? node)
       ;; hi or lo, do nothing
       g

       (symbol? node)
       ;; wire, carry the value over from predecessor
       (let [pred (first (ug/predecessors g node))
             _ (assert pred "there should be one predecessor")]
         (ug/add-attr g node :val (ug/attr g pred :val)))

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
   (ua/topsort g)))

(comment
  (-> (->graph test-input)
      simulate
      (ug/viz-graph {:auto-label true}))

  (-> (->graph input)
      simulate
      (ug/viz-graph {:auto-label true
                     ;; :dot, :neato, :fdp, :sfdp, :twopi, or :circo
                     :layout :dot
                     :save {:filename "graph.png" :format :png}}))
  )

(defn part-1 [input]
  (let [g (simulate (->graph input))
        binary-string (->> (ug/nodes g)
                           (filter #(and (symbol? %)
                                         (= \z (first (str %)))))
                           (map (fn [n] [n (ug/attr g n :val)]))
                           (sort #(compare %2 %1))
                           (map second)
                           (apply str))]
    (Long/parseLong binary-string 2)))

(comment
  (time (part-1 input))
  ;; => 46362252142374
  )
