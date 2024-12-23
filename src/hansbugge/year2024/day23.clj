(ns hansbugge.year2024.day23
  (:require
   [clojure.string :as str]
   [hansbugge.utils :as utils]
   [ubergraph.core :as ug]
   [ubergraph.alg :as ua]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defonce input (utils/fetch-input {:year 2024 :day 23}))
(def test-input "kh-tc\nqp-kh\nde-cg\nka-co\nyn-aq\nqp-ub\ncg-tb\nvc-aq\ntb-ka\nwh-tc\nyn-cg\nkh-ub\nta-co\nde-co\ntc-td\ntb-wq\nwh-td\nta-ka\ntd-qp\naq-cg\nwq-ub\nub-vc\nde-ta\nwq-aq\nwq-vc\nwh-yn\nka-de\nkh-ta\nco-tc\nwh-qp\ntb-vc\ntd-yn")

(defn parse [input]
  (->> (str/split-lines input)
       (map #(str/split % #"-"))
       (apply ug/graph)))

(defn part-1 [input]
  (let [g (parse input)]
    (->> (for [n1 (ug/nodes g)
               :when (= \t (first n1))
               :let [ns (set (ug/neighbors g n1))]
               n2 ns
               n3 (ug/neighbors g n2)
               :when (ns n3)]
           #{n1 n2 n3})
         set
         count)))

(comment
  (part-1 test-input)
  ;; => 7
  (part-1 input)
  ;; => 1098
  )

(defn part-2 [input]
  (->> (parse input)
       ua/maximal-cliques
       (apply max-key count)
       sort
       (str/join ",")))

(comment
  (part-2 test-input)
  ;; => "co,de,ka,ta"
  (part-2 input)
  ;; => "ar,ep,ih,ju,jx,le,ol,pk,pm,pp,xf,yu,zg"
)
