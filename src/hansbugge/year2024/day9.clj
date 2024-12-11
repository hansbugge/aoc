(ns hansbugge.year2024.day9
  (:require
   [hansbugge.utils :as utils]
   [medley.core :as m]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defonce input (utils/fetch-input {:year 2024 :day 9}))
(def test-input "2333133121414131402")

(defn parse-with-idx [input]
  (let [parsed (into [] (map parse-long) (re-seq #"\d" input))]
    (map vector parsed (interleave (range) (repeat nil)))))

(defn blocks [parsed]
  (reduce (fn [acc [size id]] (into acc (repeat size id))) []
          parsed))

(defn defrag-step [blx]
  (if-let [x (peek blx)]
    (let [i (.indexOf ^java.util.List (pop blx) nil)]
      (if (neg-int? i)
        blx
        ;; put last val into the place of the first nil
        (assoc (pop blx) i x)))
    ;; if last is nil, remove it
    (pop blx)))

(defn defrag [blx]
  (loop [blx blx]
    (if (every? some? blx)
      blx
      (recur (defrag-step blx)))))

(defn checksum [blx]
  (->> (map (fnil * 0) blx (range))
       (apply +)))

(defn part-1 [input]
  (->> (parse-with-idx input)
       blocks
       defrag
       checksum))

(comment
  (part-1 test-input)
  ;; => 1928

  (time (part-1 input))
  ;; "Elapsed time: 22116.423917 msecs"
  ;; => 6349606724455
  )

(defn parse [input]
  (let [parsed (sequence (comp (map (comp parse-long str))) input)]
    (loop [[^long size-file ^long size-free & xs] parsed
           pos 0
           v 0
           res []]
      (if size-file
        (recur xs (+ pos size-file (or size-free 0)) (inc v)
               (-> res
                   (conj {:type :file
                          :val v
                          :size size-file})
                   (cond-> size-free
                     (conj {:type :free
                            :size size-free}))))
        res))))

(defn write [blocks]
  (->> blocks
       (mapcat #(repeat (:size %)
                        (:val %)))
       (into [])))

(defn find-free [blocks block]
  (->> blocks
       (map vector (range))
       (m/find-first (fn [[_ {:keys [type ^long size]}]]
                       (and (= :free type)
                            (<= ^long (:size block) size))))))

(defn find-applicable [blocks]
  (loop [blocks blocks
         idx (dec (count blocks))]
    (when-let [block (peek blocks)]
      (if (= :file (:type block))
        (if-let [free (find-free blocks block)]
          [free [idx block]]
          (recur (pop blocks) (dec idx)))
        (recur (pop blocks) (dec idx))))))

(defn do-swap [blocks [[^long free-idx free-block] [^long idx block]]]
  (into (subvec blocks 0 free-idx)
        cat
        (let [free-size (- ^long (:size free-block) ^long (:size block))]
          [[block
            (conj (assoc free-block :size free-size))]
           (subvec blocks (inc free-idx) idx #_(dec (count blocks)))
           [{:type :free :size (:size block)}]
           (subvec blocks (inc idx))])))

(defn defrag-2 [blocks]
  (if-let [app (find-applicable blocks)]
    (recur (do-swap blocks app))
    blocks))

(defn part-2 [input]
  (-> (parse input)
      defrag-2
      write
      checksum))
(comment
  (part-2 test-input)
  ;; => 2858

  (part-2 input)
  ;; takes a long time...
  ;; => 6376648986651
  )
