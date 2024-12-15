(ns hansbugge.year2024.day14
  (:require
   [clojure.math :as math]
   [clojure.math.combinatorics :as combo]
   [clojure.string :as str]
   [hansbugge.utils :as utils]
   [medley.core :as m]
   [net.cgrand.xforms :as xf]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defonce input (utils/fetch-input {:year 2024 :day 14}))
(def test-input "p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3")

(defn parse [input]
  (->> (utils/numbers input)
       (mapv #(partition 2 %)))
  )

(parse test-input)

(defn simulate [[pos vel] ^long seconds size]
  (map #(mod %1 %2)
       (map + pos (map #(* seconds ^long %) vel))
       size))

(simulate (first (parse test-input)) 100 [11 10])
(simulate [[2 4] [2 -3]] 5 [11 7])

(defn quadrant-count [[^long max-x ^long max-y] ps]
  (let [half-x (quot max-x 2)
        half-y (quot max-y 2)]
    (reduce (fn [acc [^long x ^long y]]
              (cond-> acc
                (and (< x half-x) (< y half-y)) (update 0 inc)
                (and (< half-x x) (< y half-y)) (update 1 inc)
                (and (< x half-x) (< half-y y)) (update 2 inc)
                (and (< half-x x) (< half-y y)) (update 3 inc)))
            [0 0 0 0]
            ps)))

(defn part-1 [input size]
  (->> (parse input)
       (map #(simulate % 100 size))
       (quadrant-count size)
       (apply *)))

(comment
  (part-1 test-input [11 7])
  ;; => 12
  (part-1 input [101 103])
  ;; => 214400550
  )

(defn to-grid [[max-x max-y] ps]
  (let [frqs (frequencies ps)
        base-grid-row (into [] (repeat max-y " "))
        base-grid (into [] (repeat max-x base-grid-row))]
    (reduce-kv (fn [acc k v]
                 (assoc-in acc k v))
               base-grid
               frqs)))

(defn print-grid [g]
  (let [g (apply map vector g)]
    (doseq [row g]
      (doseq [point row]
        (print point))
      (prn)
      (flush))))
;; (print-grid (to-grid [11 7] [[2 4]]))

(def parsed (parse input))
(def size [101 103])

(defn visualize [t]
  (->> parsed
       (map #(simulate % t size))
       (to-grid size)
       print-grid))

(comment
  (visualize 100)
  )

(comment
  ;;; Look for a configuration where all robots are in unique positions
  (m/find-first
   (fn [t] (every?
            #{1}
            (->> parsed
                 (map #(simulate % t size))
                 frequencies
                 vals)))
   (range (* 101 103)))
  ;; => 8149

  (visualize 8149)
  ;;; That turned out to be the answer ¯\_(ツ)_/¯
  )

;;; Alternative solution? Average distance to other points.
(defn avg-dist-at [t]
  (into []
        (comp
         (map (fn [[p1 p2]]
                (->> (map #(math/pow (- ^long %1 ^long %2) 2) p1 p2)
                     (apply +)
                     (math/sqrt))))
         (xf/transjuxt {:avg xf/avg :sd xf/sd}))
        (combo/combinations
         (->> parsed
              (map #(simulate % t size)))
         2)))

(comment
  (defonce distance-data
    (time (mapv (juxt identity avg-dist-at) (range (apply * size)))))
  ;; "Elapsed time: 793519.357792 msecs"
  ;; ouch

  (first distance-data)
  ;; => [0 [{:avg 52.71799953882393, :sd 25.081497402668536}]]
  (take 5 (sort-by (comp :avg first second) distance-data))
  ;; => ([8149 [{:avg 30.930070276906157, :sd 21.632297969509644}]]
  ;;     [1557 [{:avg 41.938004750651785, :sd 22.796707921666414}]]
  ;;     [4544 [{:avg 42.008895816590154, :sd 22.69128075957332}]]
  ;;     [7428 [{:avg 42.14453119496352, :sd 22.90313787375575}]]
  ;;     [10312 [{:avg 42.20976220082274, :sd 22.899663753425443}]])
  ;; this also got us the right answer right away
  ;; but it was an expensive calculation
  )

;;; Alternative solution?  Measure entropy in configuration.
;;; As a proxy, see how much we can compress the frames.

(comment
  (def zipped-sizes
    (time (into []
                (map (fn [t]
                       (with-open [baos (java.io.ByteArrayOutputStream.)
                                   gzip (java.util.zip.GZIPOutputStream. baos)
                                   out (clojure.java.io/writer gzip)]
                         (binding [*out* out]
                           (->> parsed
                                (map #(simulate % t size))
                                (to-grid size)
                                print-grid))
                         (.finish gzip)
                         (.size baos))))
                (range (apply * size)))))
  ;; "Elapsed time: 39602.448333 msecs"
  ;; doable!

  (->> zipped-sizes
       (map-indexed (fn [idx size] {:time idx :gzip-size size}))
       (sort-by :gzip-size)
       (take 10))
  ;; => ({:time 8149, :gzip-size 554}
  ;;     {:time 7222, :gzip-size 747}
  ;;     {:time 5059, :gzip-size 752}
  ;;     {:time 2587, :gzip-size 754}
  ;;     {:time 4853, :gzip-size 754}
  ;;     {:time 8252, :gzip-size 756}
  ;;     {:time 12, :gzip-size 757}
  ;;     {:time 8561, :gzip-size 757}
  ;;     {:time 9591, :gzip-size 757}
  ;;     {:time 6501, :gzip-size 758})
  ;; easily finds the right answer!
  )


;;; More "correct" entropy solution:

;; from https://gist.github.com/kyptin/c3cb36c350885b06590c31bb97d07c23
(defn shannon-entropy [xs]
  (let [freqs (vals (frequencies xs))
        ^double n (reduce + 0 freqs)
        log2 #(/ (math/log %) (math/log 2))
        p #(double (/ ^double % n))
        summand (fn [freq]
                  (let [^double p (p freq)]
                    (* p ^double (log2 p))))]
    (reduce - 0 (map summand freqs))))


(comment
  (def entropies
    (time (->>
           (range (apply * size))
           (map-indexed (fn [idx t]
                          {:time idx
                           :entropy (->> parsed
                                         (mapcat #(simulate % t size))
                                         shannon-entropy)}))
           (into []))))
  ;; "Elapsed time: 3631.21275 msecs"
  ;; much better!

  (xf/some (comp (map :entropy) xf/avg) entropies)
  ;; => 6.604331321648145

  (->> entropies
       (sort-by :entropy)
       (take 10))
  ;; => ({:time 8149, :entropy 5.812477920648892}
  ;;     {:time 6129, :entropy 6.316580371185709}
  ;;     {:time 7139, :entropy 6.317575832754934}
  ;;     {:time 7428, :entropy 6.320254159288631}
  ;;     {:time 2587, :entropy 6.326179065494208}
  ;;     {:time 7644, :entropy 6.327381091126316}
  ;;     {:time 8767, :entropy 6.329021027017188}
  ;;     {:time 7240, :entropy 6.333596410199169}
  ;;     {:time 5059, :entropy 6.333801504367159}
  ;;     {:time 8870, :entropy 6.334892295533472})
  )

;;;; Visualization with Swing!

(import [javax.swing JFrame JLabel])

(defn text-for-time [t]
  (str "<html><pre>"
       (->> parsed
            (map #(simulate % t size))
            (to-grid size)
            print-grid
            with-out-str)
       "</pre></html>"))

(defn gui-show-at-time [^JLabel label t]
  (.setText label (text-for-time t)))

(defn show-viz []
  (let [frame (JFrame.)
        label (JLabel.)
        time (atom 0)
        speed (atom 25)
        play-pause-fn (atom nil)
        play-fn (fn play-fn []
                  (let [running (atom true)]
                    (future
                      (while @running
                        (.setTitle frame (str @time))
                        (gui-show-at-time label (swap! time inc))
                        (Thread/sleep ^long @speed)))
                    (reset! play-pause-fn
                            #(do (reset! running false)
                                 (reset! play-pause-fn play-fn)))))
        ]
    (reset! play-pause-fn play-fn)
    (doto label
      (.setFont (java.awt.Font. "Courier" java.awt.Font/PLAIN 8)))
    (doto frame
      (.setSize 600 1200)
      (.add label)
      (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
      (.setVisible true))

    (gui-show-at-time label @time)
    (.setTitle frame (str @time))

    {:frame frame
     :label label
     :play-pause (fn [] (@play-pause-fn))
     :speed speed
     :time time}))

(comment
  (def *viz (show-viz))

  ((:play-pause *viz))

  (reset! (:time *viz) 8140)
  (reset! (:speed *viz) 200)
  (.dispose (:frame *viz))
  )
