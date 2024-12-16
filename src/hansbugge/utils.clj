(ns hansbugge.utils
  (:require
   [babashka.http-client :as http]
   [babashka.fs :as fs]
   [clojure.string :as str]
   [medley.core :as m]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defonce -session-cookie
  (str "session=" (-> (slurp "session.txt") str/trim)))

(defn fetch-input [{:keys [year day]
                    :or {year 2024}}]
  (let [url (format "https://adventofcode.com/%s/day/%s/input" year day)]
    (-> (http/get url {:headers {"Cookie" -session-cookie}})
        :body)))

(comment
  (println (fetch-input {:year 2023 :day 1})))

;;; Problems with lines of integers
(defn numbers [input]
  (->> (str/split-lines input)
       (map #(map parse-long (re-seq #"-?\d+" %)))))

;;; Grid type problems

(defn grid
  ([input] (grid input identity))
  ([input char-parser]
   (into []
         (comp
          (map #(map char-parser %))
          (map vec))
         (str/split-lines input))))

(comment
  (grid "abc\ndef\nghi")
  (grid "012\n345\n678" (comp parse-long str))
  )

(def all-directions #{:n :s :e :w :ne :nw :se :sw})

(defn step [[^long x ^long y] direction]
  (case direction
    :n [(dec x) y]
    :s [(inc x) y]
    :e [x (inc y)]
    :w [x (dec y)]
    :ne [(dec x) (inc y)]
    :nw [(dec x) (dec y)]
    :se [(inc x) (inc y)]
    :sw [(inc x) (dec y)]))

(def opposite
  {:n :s
   :s :n
   :e :w
   :w :e
   :ne :sw
   :nw :se
   :se :nw
   :sw :ne})

(defn points
  "All points in a grid, left-to-right, up-to-down."
  [g]
  (for [x (range (count g))
        y (range (count (first g)))]
    [x y]))

(defn within-grid? [g [x y]]
  (let [h (count g)
        w (count (first g))]
    (and (< -1 x h)
         (< -1 y w))))

(defn flood-fill
  "Stack based flood-fill algorithm

  start-point is the origin of the search, a coordinate pair of longs.

  f : State -> Point -> State is a state function taking the current state,
  the current point deemed 'inside', and returns a new state for the remaining
  calculation.

  begin-state is the state the search begins with.

  inside-pred : State -> Point -> Bool takes the current state and the current
  point and decides whether the point is 'inside' the area to be filled.

  fuel is the number of iterations to perform before giving up.
  "
  ([start-point f begin-state inside-pred]
   (flood-fill start-point f begin-state inside-pred (long 1e6)))
  ([start-point f begin-state inside-pred start-fuel]
   (loop [stack (conj (m/queue) start-point)
          state begin-state
          ^long fuel start-fuel]
     (if (zero? fuel)
       [::out-of-fuel start-fuel stack state]
       (if-some [point (peek stack)]
         (if (inside-pred state point)
           (recur (into (pop stack)
                        (map #(step point %))
                        [:n :e :s :w])
                  (f state point)
                  (dec fuel))
           (recur (pop stack) state (dec fuel)))
         state)))))

(comment
  (flood-fill [0 0]
              (fn [state p] (m/update-existing-in state p (constantly :fill)))
              [[:A :A] [:A :B]]
              (fn [g p] (= :A (get-in g p))))
  ;; => [[:fill :fill]
  ;;     [:fill :B]]
  )

(defn print-grid [g]
  (doseq [row g]
    (doseq [c row]
      (print c))
    (prn)
    (flush)))

;;; Parallelisation helpers

(defn partition-ncpus [coll]
  (let [cnt (count coll)
        ncpus (.availableProcessors (Runtime/getRuntime))
        q (quot cnt ncpus)]
    (partition-all (if (zero? (rem cnt ncpus)) q (inc q)) coll)))

;;; Templating

(defn -day-content [{:keys [year day]
                     :or {year 2024}}]
  (let [template "(ns hansbugge.year%s.day%s
  (:require
   [clojure.string :as str]
   [hansbugge.utils :as utils]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defonce input (utils/fetch-input {:year %s :day %s}))
(def test-input \"\")

(defn part-1 [input])

(comment
  (part-1 test-input)
  (part-1 input)
  )

(defn part-2 [input])

(comment
  (part-2 test-input)
  (part-2 input)
  )
"]
    (format template year day year day)))

(defn generate-day [{:keys [year day]
                     :or {year 2024}}]
  (let [path (format "src/hansbugge/year%s/day%s.clj" year day)]
    (when (fs/exists? path)
      (throw (ex-info "Path already exists" {:path path})))
    (fs/create-dirs (fs/parent path))
    (spit path (-day-content {:year year :day day}))))

(comment
  (generate-day {:year 2024 :day 2})
  )

(defn generate-today []
  (let [today (java.time.LocalDate/now)
        m (.getMonth today)
        d (.getDayOfMonth today)
        y (.getYear today)]
    (assert (= java.time.Month/DECEMBER m))
    (generate-day {:year y :day d})))

(comment
  (generate-today)
  )
