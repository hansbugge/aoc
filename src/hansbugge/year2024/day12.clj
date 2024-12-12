(ns hansbugge.year2024.day12
  (:require
   [hansbugge.utils :as utils]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defonce input (utils/fetch-input {:year 2024 :day 12}))

(def test-input "AAAA
BBCD
BBCC
EEEC")

(defn neighbors [p]
  (mapv #(utils/step p %) [:n :e :s :w]))

(defn measure-region [grid start-point score-fn]
  (let [plant (get-in grid start-point)
        {:keys [region] :as state}
        (utils/flood-fill start-point
                          (fn [state p]
                            (let [perimeter-points (->> (neighbors p)
                                                        (remove #(= plant (get-in grid %)))
                                                        (map (fn [np] [p np])))]
                              (-> state
                                  (update :region conj p)
                                  (update :area (fnil inc 0))
                                  (update :perimeter into perimeter-points))))
                          {:region #{}
                           :perimeter #{}}
                          (fn [{:keys [region]} p]
                            (and (not (region p))
                                 (= plant (get-in grid p)))))]
    {:score (score-fn state)
     :region region}))

(defn score-1 [{:keys [^long area perimeter]}]
  (* (count perimeter) area))

(comment
  (measure-region (utils/grid "AAB\nAAC") [0 1] score-1)
  ;; => {:score 32, :region #{[0 0] [1 0] [1 1] [0 1]}}
  )

(defn measure-regions [input score-fn]
  (let [g (utils/grid input)]
    (loop [remaining-points (set (utils/points g))
           res 0]
      (if-let [current-point (first remaining-points)]
        (let [{:keys [^long score region]} (measure-region g current-point score-fn)]
          (recur (apply disj remaining-points region)
                 (+ res score)))
        res))))

(defn part-1 [input]
  (measure-regions input score-1))

(comment
  (part-1 test-input)
  ;; => 140
  (time (part-1 input))
  ;; "Elapsed time: 58.09775 msecs"
  ;; => 1518548
  )

(defn extend-line [pss ps]
  (let [f (fn [dir] (loop [ps ps
                           res #{ps}]
                      (let [ss (mapv #(utils/step % dir) ps)]
                        (if (pss ss)
                          (recur ss (conj res ss))
                          res))))]
    (into #{} (mapcat f) [:n :w :e :s])))

(defn perimeter->sides ^long [perimeter]
  (loop [perimeter perimeter
         sides 0]
    (if-some [ps (first perimeter)]
      (recur (apply disj perimeter (extend-line perimeter ps))
             (inc sides))
      sides)))

(defn score-2 [{:keys [^long area perimeter]}]
  (* (perimeter->sides perimeter) area))

(defn part-2 [input]
  (measure-regions input score-2))

(comment
  (part-2 test-input)
  ;; => 80
  (time (part-2 input))
  ;; "Elapsed time: 73.421334 msecs"
  ;; => 909564
  )
